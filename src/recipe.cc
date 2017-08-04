#include <set>

#include <clang/AST/Expr.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Lex/Lexer.h>
#include <clang/Rewrite/Core/Rewriter.h>
#include <llvm/ADT/STLExtras.h>

#include "recipe.h"

using namespace llvm;
using namespace clang;

namespace {

constexpr char kBeforeFunctionName[] = "before";
constexpr char kAfterFunctionName[] = "after";

/// \brief Checks that recipe function has expected signature.
Error checkRecipeFunc(const FunctionDecl* func) {
  assert(func->isGlobal() && "Recipe function must be global");

  if (!func->hasBody())
    return make_error<StringError>("Recipe function must have body",
                                   inconvertibleErrorCode());

  if (!func->getReturnType().getTypePtr()->isVoidType())
    return make_error<StringError>("Recipe function must have void return type",
                                   inconvertibleErrorCode());

  const CompoundStmt* comp = dyn_cast<CompoundStmt>(func->getBody());
  if (!comp || comp->size() != 1)
    return make_error<StringError>("Recipe function must contain only one statement",
                                   inconvertibleErrorCode());

  return Error::success();
}

Expected<Recipe::ParamsMap>
compareParams(const FunctionDecl* before, const FunctionDecl* after) {
  if (before->parameters().size() != after->parameters().size())
    return make_error<StringError>("Recipe functions have different number of "
                                   "parameters",
                                   inconvertibleErrorCode());

  Recipe::ParamsMap params;
  params.reserve(before->param_size());

  for (size_t i = 0, count = before->param_size(); i < count; ++i) {
    if (before->getParamDecl(i)->getOriginalType() !=
        after->getParamDecl(i)->getOriginalType()) {
      return make_error<StringError>("Different param type",
                                     inconvertibleErrorCode());
    }

    params.try_emplace(before->getParamDecl(i), after->getParamDecl(i));
  }

  return params;
}

class ParamRefVisitor : public clang::RecursiveASTVisitor<ParamRefVisitor> {
public:
  ParamRefVisitor(const std::set<const ParmVarDecl*>& params,
                  std::vector<std::tuple<const DeclRefExpr*, const ParmVarDecl*>>& refs)
      : params_(params), refs_(refs) {}

  bool VisitDeclRefExpr(DeclRefExpr* expr) {
    if (const ParmVarDecl* param_decl = dyn_cast<ParmVarDecl>(expr->getDecl())) {
      auto it = params_.find(param_decl);
      assert(it != params_.end() && "Recipe function may refer to params only");
      refs_.emplace_back(expr, param_decl);
    }

    return false;
  }

 private:
  const std::set<const ParmVarDecl*>& params_;
  std::vector<std::tuple<const DeclRefExpr*, const ParmVarDecl*>>& refs_;
};


}

// static
Expected<Recipe> Recipe::create(std::unique_ptr<ASTUnit> unit) {
  // Search top-level declarations in main file for `before` and `after`
  // functions.
  const FunctionDecl* before_func = nullptr;
  const FunctionDecl* after_func = nullptr;

  for (auto it = unit->top_level_begin(), end = unit->top_level_end();
       it != end; ++it) {
    const Decl* decl = *it;

    const FunctionDecl* func = dyn_cast<FunctionDecl>(decl);
    if (!func)
      continue;

    bool in_main_file = unit->getSourceManager().isWrittenInMainFile(func->getLocation());
    if (!in_main_file)
      continue;

    if (func->getName() == kBeforeFunctionName) {
      if (before_func)
        return make_error<StringError>("Duplicate 'before' function",
                                       inconvertibleErrorCode());
      before_func = func;

    } else if (func->getName() == kAfterFunctionName) {
      if (after_func)
        return make_error<StringError>("Duplicate 'after' function",
                                       inconvertibleErrorCode());
      after_func = func;
    }
  }

  if (!before_func)
    return make_error<StringError>("No 'before' function found",
                                   inconvertibleErrorCode());
  if (!after_func)
    return make_error<StringError>("No 'after' function found",
                                   inconvertibleErrorCode());

  // Both functions are found, check requirements
  if (Error err = checkRecipeFunc(before_func))
    return std::move(err);

  if (Error err = checkRecipeFunc(after_func))
    return std::move(err);

  // Compare that argument's types are identical.
  Expected<Recipe::ParamsMap> params = compareParams(before_func, after_func);
  if (Error err = params.takeError())
    return std::move(err);

  return Recipe(std::move(unit), before_func, after_func, std::move(*params));
}

Recipe::Recipe(std::unique_ptr<clang::ASTUnit> unit,
               const clang::FunctionDecl* before_func,
               const clang::FunctionDecl* after_func,
               ParamsMap params)
    : unit_(std::move(unit)), before_func_(before_func),
      after_func_(after_func), params_(std::move(params)) {
}

bool Recipe::matches(clang::Stmt* stmt, ASTContext& context,
                     Rewriter& rewriter) const {
  ArgsMap args;

  Stmt* before_stmt = *(cast<CompoundStmt>(before_func_->getBody())->child_begin());
  if (!stmtMatches(before_stmt, stmt, &args))
    return false;

  // Find all references to parameters in body of 'after' function.
  Stmt* after_stmt = *(cast<CompoundStmt>(after_func_->getBody())->child_begin());
  std::set<const ParmVarDecl*> after_params;
  for (auto p : params_)
    after_params.insert(p.second);
  std::vector<std::tuple<const DeclRefExpr*, const ParmVarDecl*>> refs;
  ParamRefVisitor refs_visitor(after_params, refs);
  refs_visitor.TraverseStmt(after_stmt);

  // Now replace text
  std::string repl;
  SourceLocation start = after_stmt->getLocStart();
  for (auto ref : refs) {
    SourceLocation end = std::get<0>(ref)->getLocStart().getLocWithOffset(-1);

    repl.append(Lexer::getSourceText(CharSourceRange::getTokenRange(start, end), unit_->getSourceManager(), unit_->getLangOpts()));

    const ParmVarDecl* after_param = std::get<1>(ref);
    assert(after_params != nullptr);
    const Expr* arg = args.lookup(after_param);
    assert(arg != nullptr);
    repl.append(Lexer::getSourceText(CharSourceRange::getTokenRange(arg->getExprLoc()), context.getSourceManager(), context.getLangOpts()));

    start = std::get<0>(ref)->getLocEnd().getLocWithOffset(1);
  }
  repl.append(Lexer::getSourceText(CharSourceRange::getTokenRange(start, after_stmt->getLocEnd()), unit_->getSourceManager(), unit_->getLangOpts()));

  rewriter.ReplaceText(stmt->getSourceRange(), repl);
  return true;
}

bool Recipe::stmtMatches(const Stmt* pattern, const Stmt* stmt,
                         ArgsMap* args) const {
  if (const DeclRefExpr* decl_ref = dyn_cast<DeclRefExpr>(pattern)) {
    // Pattern references some variable, it may be parameter of `before`
    // recipe function.
    if (const ParmVarDecl* param_decl = dyn_cast<ParmVarDecl>(decl_ref->getDecl())) {
      ParamsMap::const_iterator it = params_.find(param_decl);
      assert(it != params_.end() && "Recipe function may refer to params only");
      const ParmVarDecl* param = it->second;

      // Corresponding `stmt` must be expression, and it's type must match
      // that of recipe param.
      const Expr* expr = dyn_cast<Expr>(stmt);
      if (expr && expr->getType().getAsString() == param->getOriginalType().getAsString()) {
        if (args->try_emplace(param, expr).second)
          return true;

        // There is already argument for this param. We have to check whether
        // saved and current arguments are identical, but it isn't supported
        // right now.
        errs() << "warning: Duplicate param usage\n";
        return false;
      }
    }
  }

  if (pattern->getStmtClass() != stmt->getStmtClass())
    return false;

  // Use `return` inside branch when all conditions are checked and
  // there is no to match children, `break` to match children.
  switch (pattern->getStmtClass()) {
    default:
      // Be on safe side: report statements of unknown kind as unmatched.
      return false;

    case Stmt::CallExprClass:
    case Stmt::ArraySubscriptExprClass:
    case Stmt::OMPArraySectionExprClass:
    case Stmt::ImplicitCastExprClass:
    case Stmt::ParenExprClass:
    case Stmt::BreakStmtClass:
    case Stmt::ContinueStmtClass:
    case Stmt::NullStmtClass:
      return true;

    case Stmt::CXXMemberCallExprClass:
      break;

    case Stmt::CXXOperatorCallExprClass: {
      const CXXOperatorCallExpr *pop = cast<CXXOperatorCallExpr>(pattern);
      const CXXOperatorCallExpr *sop = cast<CXXOperatorCallExpr>(stmt);
      if (pop->getOperator() != sop->getOperator())
        return false;
      break;
    }

    case Stmt::CStyleCastExprClass: {
      const CStyleCastExpr* pexpr = cast<CStyleCastExpr>(pattern);
      const CStyleCastExpr* sexpr = cast<CStyleCastExpr>(stmt);
      if (pexpr->getTypeAsWritten() != sexpr->getTypeAsWritten())
        return false;
      break;
    }

    case Stmt::ReturnStmtClass: {
      const ReturnStmt* pstmt = cast<ReturnStmt>(pattern);
      const ReturnStmt* sstmt = cast<ReturnStmt>(stmt);
      return stmtMatches(pstmt->getRetValue(), sstmt->getRetValue(), args);
    }

    case Stmt::ForStmtClass: {
      const ForStmt* pfor = cast<ForStmt>(pattern);
      const ForStmt* sfor = cast<ForStmt>(stmt);

      return stmtMatches(pfor->getInit(), sfor->getInit(), args) &&
             stmtMatches(pfor->getCond(), sfor->getCond(), args) &&
             stmtMatches(pfor->getInc(), sfor->getInc(), args) &&
             stmtMatches(pfor->getBody(), sfor->getBody(), args);
    }

    case Stmt::DoStmtClass: {
      const DoStmt* pdo = cast<DoStmt>(pattern);
      const DoStmt* sdo = cast<DoStmt>(stmt);

      return stmtMatches(pdo->getCond(), sdo->getCond(), args) &&
             stmtMatches(pdo->getBody(), sdo->getBody(), args);
    }

    case Stmt::WhileStmtClass: {
      const WhileStmt* pwhile = cast<WhileStmt>(pattern);
      const WhileStmt* swhile = cast<WhileStmt>(stmt);

      return stmtMatches(pwhile->getCond(), swhile->getCond(), args) &&
             stmtMatches(pwhile->getBody(), swhile->getBody(), args);
    }

    case Stmt::IfStmtClass: {
      const IfStmt* pif = cast<IfStmt>(pattern);
      const IfStmt* sif = cast<IfStmt>(stmt);

      return stmtMatches(pif->getCond(), sif->getCond(), args) &&
             stmtMatches(pif->getThen(), sif->getThen(), args) &&
             stmtMatches(pif->getElse(), sif->getElse(), args);
    }

    case Stmt::CompoundStmtClass: {
      const CompoundStmt* pcomp = cast<CompoundStmt>(pattern);
      const CompoundStmt* scomp = cast<CompoundStmt>(stmt);

      if (pcomp->size() != scomp->size())
        return false;

      CompoundStmt::const_body_iterator pi = pcomp->body_begin();
      CompoundStmt::const_body_iterator si = scomp->body_begin();
      for (; pi != pcomp->body_end() && si != scomp->body_end(); ++pi, ++si) {
        if (!stmtMatches(*pi, *si, args))
          return false;
      }
      return true;
    }

    case Stmt::CompoundAssignOperatorClass:
    case Stmt::BinaryOperatorClass: {
      const BinaryOperator* pop = cast<BinaryOperator>(pattern);
      const BinaryOperator* sop = cast<BinaryOperator>(stmt);
      if (pop->getOpcode() != sop->getOpcode())
        return false;
      break;
    }

    case Stmt::CharacterLiteralClass: {
      const CharacterLiteral* pchar = cast<CharacterLiteral>(pattern);
      const CharacterLiteral* schar = cast<CharacterLiteral>(stmt);
      return pchar->getValue() == schar->getValue();
    }

    case Stmt::DeclRefExprClass: {
      const DeclRefExpr* pdecl = cast<DeclRefExpr>(pattern);
      const DeclRefExpr* sdecl = cast<DeclRefExpr>(stmt);
      return pdecl->getDecl() == sdecl->getDecl();
    }

    case Stmt::IntegerLiteralClass: {
      const IntegerLiteral* pint = cast<IntegerLiteral>(pattern);
      const IntegerLiteral* sint = cast<IntegerLiteral>(stmt);

      llvm::APInt pi = pint->getValue();
      llvm::APInt si = sint->getValue();
      return pi.getBitWidth() == si.getBitWidth() && pi == si;
    }

    case Stmt::FloatingLiteralClass: {
      const FloatingLiteral* pfloat = cast<FloatingLiteral>(pattern);
      const FloatingLiteral* sfloat = cast<FloatingLiteral>(stmt);
      return pfloat->getValue().bitwiseIsEqual(sfloat->getValue());
    }

    case Stmt::StringLiteralClass: {
      const clang::StringLiteral* pstring = cast<clang::StringLiteral>(pattern);
      const clang::StringLiteral* sstring = cast<clang::StringLiteral>(stmt);
      return pstring->getBytes() == sstring->getBytes();
    }

    case Stmt::MemberExprClass: {
      const MemberExpr* pmember = cast<MemberExpr>(pattern);
      const MemberExpr* smember = cast<MemberExpr>(stmt);
      if (pmember->getMemberDecl()->getDeclName().getAsString() !=
          smember->getMemberDecl()->getDeclName().getAsString())
        return false;
      break;
    }

    case Stmt::UnaryOperatorClass: {
      const UnaryOperator *punary = cast<UnaryOperator>(pattern);
      const UnaryOperator *sunary = cast<UnaryOperator>(stmt);
      if (punary->getOpcode() != sunary->getOpcode())
        return false;
      break;
    }
  }

  const Expr *pexpr = cast<Expr>(pattern);
  const Expr *sexpr = cast<Expr>(stmt);

  Expr::const_child_iterator pi = pexpr->child_begin();
  Expr::const_child_iterator si = sexpr->child_begin();
  for (; pi != pexpr->child_end() && si != sexpr->child_end(); ++pi, ++si) {
    if (!*pi || !*si || !stmtMatches(*pi, *si, args))
      return false;
  }

  // If there are different number of children in the statements, return
  // false.
  if (pi != pexpr->child_end() || si != sexpr->child_end())
    return false;

  return true;
}
