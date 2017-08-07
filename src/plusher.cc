#include <string>
#include <iostream>

#include "lib.h"

int main(int argc, const char **argv) {
  std::string result;
  if (ProcessFile(argc, argv, &result))
    std::cout << result;
}
