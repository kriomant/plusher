.PHONY: compile run

compile:
	ninja -C out plusher

run: compile
	LD_LIBRARY_PATH=/usr/local/Cellar/llvm/4.0.1/lib ./out/plusher -recipe=receipt.cc test.cc -- -std=c++11 -I/usr/local/opt/llvm/include/c++/v1 -I/usr/local/Cellar/llvm/4.0.1/lib/clang/4.0.1/include
