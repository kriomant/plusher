#include <cstdio>
#include <fstream>
#include <string>
#include <gtest/gtest.h>
#include <clang/Tooling/CompilationDatabase.h>

#include "lib.h"

namespace {

bool EndsWith(const std::string& where, const std::string& what) {
  return where.size() >= what.size() &&
         where.substr(where.size() - what.size()) == what;
}

struct Case {
  std::string source;
  std::string expected;
};

Case ParseCase(const char* diff) {
  std::stringstream ss(diff);
  Case c;

  size_t indent = 0;
  std::string line;
  while(std::getline(ss, line, '\n')){
    if (line.empty()) {
      c.source.append("\n");
      c.expected.append("\n");
      continue;
    }

    if (indent == 0) {
      indent = line.find_first_not_of(' ');
    }

    if (EndsWith(line, " >>>")) {
      c.source.append(line.substr(indent, line.size() - indent - 4));
      c.source.append("\n");
    } else if (EndsWith(line, " <<<")) {
      c.expected.append(line.substr(indent, line.size() - indent - 4));
      c.expected.append("\n");
    } else if (line.size() < indent) {
      continue;
    } else {
      c.source.append(line.substr(indent));
      c.source.append("\n");
      c.expected.append(line.substr(indent));
      c.expected.append("\n");
    }
  }

  return c;
}

void TestReplace(const char* recipe, const char* change) {
  Case c = ParseCase(change);

  char _dirname[PATH_MAX];
  ASSERT_TRUE(tmpnam(_dirname));
  std::string dirname = _dirname;
  ASSERT_EQ(0, mkdir(dirname.c_str(), S_IRWXU));

  std::string recipe_file = dirname + "/recipe.cc";
  std::string source_file = dirname + "/source.cc";

  std::ofstream(recipe_file) << recipe;
  std::ofstream(source_file) << c.source;

  const char* const args[] = {
    "--", "-std=c++11",
    "-I/usr/local/opt/llvm/include/c++/v1",
    "-I/usr/local/Cellar/llvm/4.0.1/lib/clang/4.0.1/include" };

  int argc = sizeof(args) / sizeof(args[0]);
  std::unique_ptr<clang::tooling::CompilationDatabase> compilations(
      clang::tooling::FixedCompilationDatabase::loadFromCommandLine(
          argc, args));

  std::string result;
  bool success = ProcessFile(*compilations, {source_file}, recipe_file,
                             &result);

  ASSERT_EQ(0, unlink(recipe_file.c_str()));
  ASSERT_EQ(0, unlink(source_file.c_str()));
  ASSERT_EQ(0, rmdir(dirname.c_str()));

  ASSERT_TRUE(success);
  EXPECT_EQ(c.expected, result);
}

TEST(ReplaceTest, SingleOccurence) {
  TestReplace(
    R"#(
      #include <string>
      void before(std::string s) {
        s.clear();
      }
      void after(std::string s) {
        std::string().swap(s);
      }
    )#",

    R"#(
      #include <string>
      int main() {
        std::string str;
        str.clear(); >>>
        std::string().swap(str); <<<
      }
    )#");
}

TEST(ReplaceTest, SeveralOccurences) {
  TestReplace(
    R"#(
      #include <string>
      void before(std::string s) {
        s.clear();
      }
      void after(std::string s) {
        std::string().swap(s);
      }
    )#",

    R"#(
      #include <string>
      int main() {
        std::string str;
        str.clear(); >>>
        std::string().swap(str); <<<
        str.clear(); >>>
        std::string().swap(str); <<<
      }
    )#");
}

TEST(ReplaceTest, Expression) {
  TestReplace(
    R"#(
      #include <string>
      bool before(std::string s) {
        return s.size() == 0;
      }
      bool after(std::string s) {
        return s.empty();
      }
    )#",

    R"#(
      #include <string>
      int main() {
        std::string str;
        bool is_empty = str.size() == 0; >>>
        bool is_empty = str.empty(); <<<
      }
    )#");
}

}  // namespace

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
