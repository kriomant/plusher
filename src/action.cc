#include "action.h"

#include "ast_consumer.h"

ReplaceActionFactory::ReplaceActionFactory(
    const Recipe &recipe, ReplacementsMap *replacements,
    std::string* new_content)
    : recipe_(recipe), replacements_(replacements), new_content_(new_content) {}

clang::FrontendAction* ReplaceActionFactory::create() {
  return new ReplaceAction(recipe_, replacements_, new_content_);
}

ReplaceAction::ReplaceAction(const Recipe &recipe,
                             ReplacementsMap *replacements,
                             std::string* new_content)
    : recipe_(recipe), replacements_(replacements), new_content_(new_content) {}

std::unique_ptr<clang::ASTConsumer>
ReplaceAction::CreateASTConsumer(clang::CompilerInstance &CI,
                                 clang::StringRef file) {
  return llvm::make_unique<ReplaceASTConsumer>(CI, recipe_, replacements_,
                                               new_content_);
}
