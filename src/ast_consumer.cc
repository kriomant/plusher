#include "ast_consumer.h"

#include "ast_visitor.h"

using namespace clang;

ReplaceASTConsumer::ReplaceASTConsumer(CompilerInstance &ci,
                                       const Recipe& recipe)
    : ci_(ci), recipe_(recipe),
      rewriter_(ci_.getSourceManager(), ci_.getLangOpts()) {
}

void ReplaceASTConsumer::HandleTranslationUnit(ASTContext &Ctx) {
  FileID main_file_id = ci_.getSourceManager().getMainFileID();
  SourceLocation file_start =
      ci_.getSourceManager().getLocForStartOfFile(main_file_id);
  SourceLocation file_end =
      ci_.getSourceManager().getLocForEndOfFile(main_file_id);
  llvm::outs() << rewriter_.getRewrittenText(SourceRange(file_start, file_end));
}

bool ReplaceASTConsumer::HandleTopLevelDecl(DeclGroupRef group) {
  ReplaceASTVisitor visitor(recipe_, ci_.getASTContext(), rewriter_);

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
