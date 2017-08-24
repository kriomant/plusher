#ifndef PLUSHER_RECIPE_H_
#define PLUSHER_RECIPE_H_

#include <memory>

#include <clang/AST/ASTContext.h>
#include <clang/Frontend/ASTUnit.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/Support/Error.h>

namespace clang {
class Rewriter;
}

class Recipe {
public:
  typedef llvm::DenseMap<const clang::NamedDecl*,
                         const clang::NamedDecl*> TemplateParamsMap;
  typedef llvm::DenseMap<const clang::ParmVarDecl*,
                         const clang::ParmVarDecl*> ParamsMap;

  static llvm::Expected<Recipe> create(std::unique_ptr<clang::ASTUnit> unit);
  bool matches(clang::Stmt* stmt, clang::ASTContext& context,
               clang::Rewriter& rewriter) const;

private:
  typedef llvm::DenseMap<const clang::ParmVarDecl*,
                         const clang::Expr*> ArgsMap;
  typedef llvm::DenseMap<const clang::NamedDecl*,
                         const clang::Type*> TemplateArgsMap;

  Recipe(std::unique_ptr<clang::ASTUnit> unit,
         const clang::FunctionDecl* before_func,
         const clang::FunctionDecl* after_func,
         ParamsMap params, TemplateParamsMap template_params);

  bool stmtMatches(const clang::Stmt* pattern, const clang::Stmt* stmt,
                   ArgsMap* args, TemplateArgsMap* template_args) const;
  bool typeMatches(clang::QualType tmpl, clang::QualType type,
                   TemplateArgsMap* template_args) const;
  bool typeMatches(const clang::Type* pattern, const clang::Type* type,
                   TemplateArgsMap* template_args) const;
  bool templateArgumentMatches(clang::TemplateArgument tmpl,
                               clang::TemplateArgument arg,
                               TemplateArgsMap* args) const;

  clang::Stmt* funcStmt(const clang::FunctionDecl* func) const;
  clang::Stmt* beforeStmt() const;
  clang::Stmt* afterStmt() const;

  std::unique_ptr<clang::ASTUnit> unit_;
  const clang::FunctionDecl* before_func_;
  const clang::FunctionDecl* after_func_;
  const ParamsMap params_;
  const TemplateParamsMap template_params_;
};

#endif  // PLUSHER_RECIPE_H_
