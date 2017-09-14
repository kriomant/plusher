#include "ast_consumer.h"

#include <clang/Rewrite/Core/Rewriter.h>
#include <clang/Tooling/Core/Replacement.h>

#include "ast_visitor.h"

using namespace clang;

ReplaceASTConsumer::ReplaceASTConsumer(
    CompilerInstance &ci, const Recipe& recipe,
    std::map<std::string, clang::tooling::Replacements>* replacements,
    std::string* new_content)
    : ci_(ci), recipe_(recipe), replacements_(replacements),
      new_content_(new_content) {
}

void ReplaceASTConsumer::HandleTranslationUnit(ASTContext &Ctx) {
  if (new_content_) {
    Rewriter rewriter(ci_.getSourceManager(), ci_.getLangOpts());
    FileID main_file_id = ci_.getSourceManager().getMainFileID();
    StringRef main_file_name =
        ci_.getSourceManager().getFileEntryForID(main_file_id)->getName();
    tooling::applyAllReplacements((*replacements_)[main_file_name], rewriter);

    SourceLocation file_start =
        ci_.getSourceManager().getLocForStartOfFile(main_file_id);
    SourceLocation file_end =
        ci_.getSourceManager().getLocForEndOfFile(main_file_id);
    *new_content_ =
        rewriter.getRewrittenText(SourceRange(file_start, file_end));
  }
}

bool ReplaceASTConsumer::HandleTopLevelDecl(DeclGroupRef group) {
  ReplaceASTVisitor visitor(recipe_, ci_.getASTContext(), replacements_);

  for (DeclGroupRef::iterator b = group.begin(), e = group.end(); b != e; ++b) {
    // Only traverse declarations made in the main file
    auto SL = (*b)->getLocation();
    SL = ci_.getSourceManager().getExpansionLoc(SL);
    if (ci_.getSourceManager().getFileID(SL) !=
        ci_.getSourceManager().getMainFileID())
      continue;

    // Traverse the declaration using our AST visitor.

    visitor.TraverseDecl(*b);
  }

  return true;
}
