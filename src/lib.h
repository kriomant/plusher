#include <string>

namespace clang { namespace tooling {
class CompilationDatabase;
}}

bool ProcessFile(int argc, const char **argv, std::string* result);

bool ProcessFile(const clang::tooling::CompilationDatabase& compilations,
                 const std::vector<std::string>& source_paths,
                 const std::string& recipe_path,
                 std::string* result);
