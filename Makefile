.PHONY: compile run compile-tests tests

compile:
	ninja -C out plusher

run: compile
	LD_LIBRARY_PATH=/usr/local/Cellar/llvm/5.0.0/lib ./out/plusher -recipe=receipt.cc test.cc -- -std=c++11 -I/usr/local/Cellar/llvm/5.0.0/include/c++/v1 -I/usr/local/Cellar/llvm/5.0.0/lib/clang/5.0.0/include

compile-tests:
	ninja -C out tests

test: compile-tests
	LD_LIBRARY_PATH=/usr/local/Cellar/llvm/5.0.0/lib ./out/tests

clean:
	ninja -C out -t clean
