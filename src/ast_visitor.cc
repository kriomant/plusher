#include "ast_visitor.h"

#include "recipe.h"

ReplaceASTVisitor::ReplaceASTVisitor(
    const Recipe &recipe, clang::ASTContext &context,
    std::map<std::string, clang::tooling::Replacements>* replacements)
    : recipe_(recipe), context_(context), replacements_(replacements)
{}

bool ReplaceASTVisitor::VisitFunctionDecl(clang::FunctionDecl* decl) {
  return true;
}

bool ReplaceASTVisitor::VisitStmt(clang::Stmt* stmt) {
  if (recipe_.tryApply(stmt, context_, replacements_)) {
    return true;
  }

  return true;
}
