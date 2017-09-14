#include <string>

namespace clang { namespace tooling {
class CompilationDatabase;
}}

bool ProcessFiles(int argc, const char **argv);

bool ProcessFiles(const clang::tooling::CompilationDatabase& compilations,
                  const std::vector<std::string>& source_paths,
                  const std::string& recipe_path,
                  std::string* result);
