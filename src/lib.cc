#include "lib.h"

#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

#include "action.h"
#include "recipe.h"

#undef NDEBUG
#include <cassert>

using namespace clang;
using namespace clang::tooling;
using namespace llvm;

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory PlusherCategory("plusher options");

static cl::opt<std::string> RecipeFile("recipe", cl::Required,
                                       cl::desc("<recipe file>"));

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...");

bool ProcessFiles(int argc, const char **argv) {
  CommonOptionsParser OptionsParser(argc, argv, PlusherCategory);
  return ProcessFiles(OptionsParser.getCompilations(),
                      OptionsParser.getSourcePathList(),
                      RecipeFile, nullptr);
}

bool ProcessFiles(const tooling::CompilationDatabase& compilations,
                  const std::vector<std::string>& source_paths,
                  const std::string& recipe_path,
                  std::string* result) {
  // Parse and analyze recipe.
  ClangTool RecipeTool(compilations,
                       ArrayRef<std::string>(recipe_path));
  std::vector<std::unique_ptr<ASTUnit>> ASTs;
  RecipeTool.buildASTs(ASTs);
  assert(ASTs.size() == 1);
  Expected<Recipe> recipe = Recipe::create(std::move(ASTs[0]));
  if (auto err = recipe.takeError()) {
    handleAllErrors(std::move(err), [&](StringError &err) {
      llvm::errs() << "Failed to compile recipe: " << err.message() << '\n';
    });
    return 1;
  }

  RefactoringTool Tool(compilations, source_paths);
  ReplaceActionFactory action_factory(*recipe, &Tool.getReplacements(), result);
  int code = Tool.run(&action_factory);
  if (code != 0)
    return false;

  if (!result) {
    // Serialization format is documented in tools/clang/scripts/run_tool.py
    llvm::outs() << "==== BEGIN EDITS ====\n";
    for (const auto& file_repls : Tool.getReplacements()) {
      for (const auto& r : file_repls.second) {
        std::string replacement_text = r.getReplacementText().str();
        std::replace(replacement_text.begin(), replacement_text.end(), '\n',
                     '\0');
        llvm::outs() << "r:::" << r.getFilePath() << ":::" << r.getOffset()
                     << ":::" << r.getLength() << ":::" << replacement_text
                     << "\n";
      }
    }
    llvm::outs() << "==== END EDITS ====\n";
  }

  return true;
}
