#ifndef PLUSHER_AST_CONSUMER_H_
#define PLUSHER_AST_CONSUMER_H_

#include <clang/AST/ASTConsumer.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Rewrite/Core/Rewriter.h>

class Recipe;

class ReplaceASTConsumer : public clang::ASTConsumer {
public:
  ReplaceASTConsumer(clang::CompilerInstance &ci, const Recipe& recipe);


  void HandleTranslationUnit(clang::ASTContext &Ctx) override;
  bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

private:
    clang::CompilerInstance &ci_;
    const Recipe& recipe_;

    clang::Rewriter rewriter_;
};

#endif  // PLUSHER_AST_CONSUMER_H_
