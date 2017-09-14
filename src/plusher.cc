#include <string>
#include <iostream>

#include "lib.h"

int main(int argc, const char **argv) {
  return ProcessFiles(argc, argv) ? 0 : 1;
}
