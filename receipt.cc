#include <string>

void before(std::string s) {
  s.clear();
}

void after(std::string s) {
  std::string().swap(s);
}

void obsolete() {
}
