#ifndef PLUSHER_RECIPE_H_
#define PLUSHER_RECIPE_H_

#include <memory>

#include <clang/AST/ASTContext.h>
#include <clang/Frontend/ASTUnit.h>
#include <clang/Tooling/Core/Replacement.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/Optional.h>
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

  bool tryApply(
      clang::Stmt* stmt, clang::ASTContext& context,
      std::map<std::string, clang::tooling::Replacements>* replacements) const;

private:
  struct BeforeFunc {
    BeforeFunc(const clang::FunctionDecl *func, ParamsMap params_map,
               TemplateParamsMap template_params_map);

    const clang::FunctionDecl *func;
    const ParamsMap params_map;
    const TemplateParamsMap template_params_map;
  };

  typedef llvm::DenseMap<const clang::ParmVarDecl*,
                         const clang::Expr*> ArgsMap;
  typedef llvm::DenseMap<const clang::NamedDecl*,
                         const clang::Type*> TemplateArgsMap;

  Recipe(std::unique_ptr<clang::ASTUnit> unit,
         std::vector<BeforeFunc> before_funcs,
         const clang::FunctionDecl* after_func);

  bool funcMatches(
      const BeforeFunc &func, clang::Stmt *stmt, clang::ASTContext &context,
      std::map<std::string, clang::tooling::Replacements>* replacements) const;
  bool stmtMatches(const clang::Stmt *pattern, const clang::Stmt *stmt,
                   const ParamsMap& params,
                   const TemplateParamsMap& template_params,
                   ArgsMap *args, TemplateArgsMap *template_args) const;
  bool typeMatches(clang::QualType tmpl, clang::QualType type,
                   const TemplateParamsMap& template_params,
                   TemplateArgsMap* template_args) const;
  bool typeMatches(const clang::Type* pattern, const clang::Type* type,
                   const TemplateParamsMap& template_params,
                   TemplateArgsMap* template_args) const;
  bool templateArgumentMatches(clang::TemplateArgument tmpl,
                               clang::TemplateArgument arg,
                               const TemplateParamsMap& template_params,
                               TemplateArgsMap* args) const;

  clang::Stmt* funcStmt(const clang::FunctionDecl* func) const;

  std::unique_ptr<clang::ASTUnit> unit_;
  const std::vector<BeforeFunc> before_funcs_;
  const clang::FunctionDecl* after_func_;
};

#endif  // PLUSHER_RECIPE_H_
