CFLAGS=-Wall -ggdb3 -std=c11
.PHONY: test
TEST_FOLDER = test

all: runner one-runner

runner: runner.c
	$(CC) $(CFLAGS) -o runner runner.c

one-runner: one-runner.c
	$(CC) $(CFLAGS) -o one-runner one-runner.c

c: runner c/core.c
	$(CC) $(CFLAGS) -o c/core c/core.c
	runner c/core

js: runner
	./runner javascript/core.js

test: $(TEST_FOLDER)/test.c
	$(CC) $(CFLAGS) -o $(TEST_FOLDER)/test $(TEST_FOLDER)/test.c
	cd $(TEST_FOLDER) && ./test

