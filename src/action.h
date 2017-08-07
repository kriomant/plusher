#ifndef PLUSHER_ACTION_H_
#define PLUSHER_ACTION_H_

#include <clang/Frontend/FrontendActions.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/Tooling.h>

class Recipe;

class ReplaceActionFactory : public clang::tooling::FrontendActionFactory {
 public:
  ReplaceActionFactory(const Recipe& recipe, std::string* result);

  clang::FrontendAction *create() override;

 private:
  const Recipe& recipe_;
  std::string* const result_;
};

// For each source file provided to the tool, a new FrontendAction is created.
class ReplaceAction : public clang::ASTFrontendAction {
 public:
  ReplaceAction(const Recipe& recipe, std::string* result);

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, clang::StringRef file) override;

 private:
  const Recipe& recipe_;
  std::string* const result_;
};

#endif  // PLUSHER_ACTION_H_
