#ifndef PLUSHER_AST_CONSUMER_H_
#define PLUSHER_AST_CONSUMER_H_

#include <map>

#include <clang/AST/ASTConsumer.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/Core/Replacement.h>

class Recipe;

class ReplaceASTConsumer : public clang::ASTConsumer {
public:
  ReplaceASTConsumer(
      clang::CompilerInstance &ci, const Recipe &recipe,
      std::map<std::string, clang::tooling::Replacements>* replacements,
      std::string* new_content);

  void HandleTranslationUnit(clang::ASTContext &Ctx) override;
  bool HandleTopLevelDecl(clang::DeclGroupRef DR) override;

private:
    clang::CompilerInstance &ci_;
    const Recipe& recipe_;

    std::map<std::string, clang::tooling::Replacements>* const replacements_;
    std::string* const new_content_;
};

#endif  // PLUSHER_AST_CONSUMER_H_
