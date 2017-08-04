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
  typedef llvm::DenseMap<const clang::ParmVarDecl*,
                         const clang::ParmVarDecl*> ParamsMap;

  static llvm::Expected<Recipe> create(std::unique_ptr<clang::ASTUnit> unit);
  bool matches(clang::Stmt* stmt, clang::ASTContext& context,
               clang::Rewriter& rewriter) const;

private:
  typedef llvm::DenseMap<const clang::ParmVarDecl*,
                         const clang::Expr*> ArgsMap;

  Recipe(std::unique_ptr<clang::ASTUnit> unit,
         const clang::FunctionDecl* before_func,
         const clang::FunctionDecl* after_func,
         ParamsMap params);

  bool stmtMatches(const clang::Stmt* pattern, const clang::Stmt* stmt,
                   ArgsMap* args) const;

  std::unique_ptr<clang::ASTUnit> unit_;
  const clang::FunctionDecl* before_func_;
  const clang::FunctionDecl* after_func_;
  const llvm::DenseMap<const clang::ParmVarDecl*, const clang::ParmVarDecl*> params_;
};

#endif  // PLUSHER_RECIPE_H_
