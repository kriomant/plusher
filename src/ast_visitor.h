#ifndef PLUSHER_AST_VISITOR_H_
#define PLUSHER_AST_VISITOR_H_

#include <clang/AST/RecursiveASTVisitor.h>

namespace clang {
class Rewriter;
}

class Recipe;

class ReplaceASTVisitor : public clang::RecursiveASTVisitor<ReplaceASTVisitor> {
public:
  ReplaceASTVisitor(const Recipe& recipe, clang::ASTContext& context,
                    clang::Rewriter& rewriter);

  bool VisitFunctionDecl(clang::FunctionDecl* decl);
  bool VisitStmt(clang::Stmt* stmt);

 private:
  const Recipe& recipe_;
  clang::ASTContext& context_;
  clang::Rewriter& rewriter_;
};

#endif  // PLUSHER_AST_VISITOR_H_
