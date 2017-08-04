#include "action.h"

#include "ast_consumer.h"

ReplaceActionFactory::ReplaceActionFactory(const Recipe& recipe)
    : recipe_(recipe) {
}

clang::FrontendAction* ReplaceActionFactory::create() {
  return new ReplaceAction(recipe_);
}

ReplaceAction::ReplaceAction(const Recipe& recipe)
    : recipe_(recipe)
{}

std::unique_ptr<clang::ASTConsumer>
ReplaceAction::CreateASTConsumer(clang::CompilerInstance &CI,
                                 clang::StringRef file) {
  return llvm::make_unique<ReplaceASTConsumer>(CI, recipe_);
}
