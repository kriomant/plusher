#ifndef PLUSHER_AST_VISITOR_H_
#define PLUSHER_AST_VISITOR_H_

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Tooling/Core/Replacement.h>

namespace clang {
class Rewriter;
}

class Recipe;

class ReplaceASTVisitor : public clang::RecursiveASTVisitor<ReplaceASTVisitor> {
public:
  ReplaceASTVisitor(
      const Recipe &recipe, clang::ASTContext &context,
      std::map<std::string, clang::tooling::Replacements>* replacements);

  bool VisitFunctionDecl(clang::FunctionDecl* decl);
  bool VisitStmt(clang::Stmt* stmt);

 private:
  const Recipe& recipe_;
  clang::ASTContext& context_;
  std::map<std::string, clang::tooling::Replacements>* const replacements_;
};

#endif  // PLUSHER_AST_VISITOR_H_
