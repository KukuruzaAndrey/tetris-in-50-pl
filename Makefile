CFLAGS=-Wall -ggdb3 -std=c11
.PHONY: test
SRCS_FOLDER = srcs
TEST_FOLDER = test
CASE_PATH = cases

all: runner one-runner

runner: runner.c
	$(CC) $(CFLAGS) -o runner runner.c

one-runner: one-runner.c
	$(CC) $(CFLAGS) -o one-runner one-runner.c

c: runner $(SRCS_FOLDER)/c
	$(CC) $(CFLAGS) -o c/core c/core.c
	runner c/core

js: runner
	./runner $(SRCS_FOLDER)/javascript/core.js

test: $(TEST_FOLDER)/test.c
	$(CC) $(CFLAGS) -o $(TEST_FOLDER)/test $(TEST_FOLDER)/test.c
	cd $(TEST_FOLDER) && ./test $(CASE_PATH)

one: one-runner
	> logs.txt
	xargs -a ./one-cases.txt -L 1 ./one-runner ./srcs/javascript/core.js
