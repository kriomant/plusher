#include "ast_visitor.h"

#include "recipe.h"

ReplaceASTVisitor::ReplaceASTVisitor(const Recipe& recipe,
                                     clang::ASTContext& context,
                                     clang::Rewriter& rewriter)
    : recipe_(recipe), context_(context), rewriter_(rewriter)
{}

bool ReplaceASTVisitor::VisitFunctionDecl(clang::FunctionDecl* decl) {
  return true;
}

bool ReplaceASTVisitor::VisitStmt(clang::Stmt* stmt) {
  if (recipe_.matches(stmt, context_, rewriter_)) {
    return true;
  }

  return true;
}
