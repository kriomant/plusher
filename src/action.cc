#include "action.h"

#include "ast_consumer.h"

ReplaceActionFactory::ReplaceActionFactory(const Recipe& recipe,
                                           std::string* result)
    : recipe_(recipe), result_(result) {
}

clang::FrontendAction* ReplaceActionFactory::create() {
  return new ReplaceAction(recipe_, result_);
}

ReplaceAction::ReplaceAction(const Recipe& recipe, std::string* result)
    : recipe_(recipe), result_(result)
{}

std::unique_ptr<clang::ASTConsumer>
ReplaceAction::CreateASTConsumer(clang::CompilerInstance &CI,
                                 clang::StringRef file) {
  return llvm::make_unique<ReplaceASTConsumer>(CI, recipe_, result_);
}
