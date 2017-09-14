#ifndef PLUSHER_ACTION_H_
#define PLUSHER_ACTION_H_

#include <clang/Frontend/FrontendActions.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/Core/Replacement.h>
#include <clang/Tooling/Tooling.h>

class Recipe;

using ReplacementsMap = std::map<std::string, clang::tooling::Replacements>;

class ReplaceActionFactory : public clang::tooling::FrontendActionFactory {
 public:
  ReplaceActionFactory(const Recipe &recipe,
                       ReplacementsMap *replacements,
                       std::string* new_content = nullptr);

   clang::FrontendAction *create() override;

 private:
  const Recipe& recipe_;
  ReplacementsMap* const replacements_;
  std::string* const new_content_;
};

// For each source file provided to the tool, a new FrontendAction is created.
class ReplaceAction : public clang::ASTFrontendAction {
 public:
  ReplaceAction(const Recipe& recipe, ReplacementsMap* replacements,
                std::string* new_content);

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, clang::StringRef file) override;

 private:
  const Recipe& recipe_;
  ReplacementsMap* const replacements_;
  std::string* const new_content_;
};

#endif  // PLUSHER_ACTION_H_
