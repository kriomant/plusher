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
    "-I/usr/local/Cellar/llvm/5.0.0/include/c++/v1",
    "-I/usr/local/Cellar/llvm/5.0.0/lib/clang/5.0.0/include" };

  int argc = sizeof(args) / sizeof(args[0]);
  std::string error;
  std::unique_ptr<clang::tooling::CompilationDatabase> compilations(
      clang::tooling::FixedCompilationDatabase::loadFromCommandLine(
          argc, args, error));

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

TEST(ReplaceTest, Subexpression) {
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
        bool is_empty = true && str.size() == 0; >>>
        bool is_empty = true && str.empty(); <<<
      }
    )#");
}

// When subexpression is replaced, it must be surrounded by braces
// if operation priorities require this.
TEST(ReplaceTest, SubexpressionPriority) {
  TestReplace(
    R"#(
      #include <string>
      bool before(std::string s) {
        return s.empty();
      }
      bool after(std::string s) {
        return s.size() == 0;
      }
    )#",

    R"#(
      #include <string>
      int main() {
        std::string str;
        bool non_empty = !str.empty(); >>>
        bool non_empty = !(str.size() == 0); <<<
      }
    )#");
}

TEST(ReplaceTest, TypeOfExpressionIsChecked) {
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
      #include <vector>
      int main() {
        std::vector<int> v;
        bool empty = v.size() == 0;
      }
    )#");
}

TEST(ReplaceTest, RecipeWithoutParameters) {
  TestReplace(
    R"#(
      #include <string>
      int before() {
        return 24;
      }
      int after() {
        return 42;
      }
    )#",

    R"#(
      #include <string>
      int main() {
        int i = 24; >>>
        int i = 42; <<<
      }
    )#");
}

TEST(ReplaceTest, Constructor) {
  TestReplace(
    R"#(
      #include <string>
      std::string before() {
        return std::string("");
      }
      std::string after() {
        return std::string();
      }
    )#",

    R"#(
      #include <string>
      int main() {
        std::string s = std::string(""); >>>
        std::string s = std::string(); <<<

        std::string s2 = std::string("ab");
        std::string s3 = std::string(2, ' ');
      }
    )#");
}

TEST(ReplaceTest, TypeParameters) {
  TestReplace(
    R"#(
      #include <vector>
      template <typename T>
      bool before(std::vector<T> v) {
        return v.size() == 0;
      }
      template <typename T>
      bool after(std::vector<T> v) {
        return v.empty();
      }
    )#",

    R"#(
      #include <vector>
      int main() {
        std::vector<int> v;
        bool is_empty = v.size() == 0; >>>
        bool is_empty = v.empty(); <<<
      }
    )#");
}

TEST(ReplaceTest, SeveralBeforeFunctions) {
  TestReplace(
    R"#(
      #include <string>
      bool before_size_is_zero(std::string s) {
        return s.size() == 0;
      }
      bool before_not_size(std::string s) {
        return !s.size();
      }
      bool after(std::string s) {
        return s.empty();
      }
    )#",

    R"#(
      #include <string>
      int main() {
        std::string v;
        bool empty = v.size() == 0 || !v.size(); >>>
        bool empty = v.empty() || v.empty(); <<<
      }
    )#");
}

}  // namespace

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
