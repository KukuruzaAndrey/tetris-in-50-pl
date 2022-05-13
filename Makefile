CFLAGS=-Wall -ggdb3 -std=c11
.PHONY: test

### DIRS ###
SRCS_DIR = srcs
TEST_DIR = test
CASE_DIR = cases

### SRC FILES ###
RUNNER = runner


all: $(RUNNER) one-runner

$(RUNNER): runner.c utils.c
	$(CC) $(CFLAGS) -o $(RUNNER) runner.c utils.c

one-runner: one-runner.c utils.c
	$(CC) $(CFLAGS) -o one-runner one-runner.c utils.c

c: $(RUNNER) $(SRCS_DIR)/c
	$(CC) $(CFLAGS) -o c/core c/core.c
	$(RUNNER) c/core

js: $(RUNNER)
	./$(RUNNER) $(SRCS_DIR)/javascript/core.js

test: $(TEST_DIR)/test.c utils.c
	$(CC) $(CFLAGS) -o $(TEST_DIR)/test $(TEST_DIR)/test.c utils.c
	
	## generate tests for move down from tick tests (results are the same, only command is diffirent)
	# copy all tick tests to down folder
	cp -r $(TEST_DIR)/$(CASE_DIR)/01_tick/* $(TEST_DIR)/$(CASE_DIR)/04_move-down
	# for all test files
	# find lines with arguments; change first '0' to '3'; write changes in-place (echo do the trick)
	for f in $$(find $(TEST_DIR)/$(CASE_DIR)/04_move-down -type f -not -name readme.txt); \
	do \
		echo "$$(awk '{if (((NR - 1) % 25 == 0) || ((NR - 2) % 25 == 0)) $$1=3; print $0}' $$f)" > $$f; \
	done
	
	cd $(TEST_DIR) && ./test $(CASE_DIR)

one: one-runner
	> logs.txt
	xargs -a ./one-cases.txt -L 1 ./one-runner ./srcs/javascript/core.js

clean:
	rm -f $(RUNNER)
	rm -f one-runner
	rm -f ./test/test
	rm -rf $(TEST_DIR)/$(CASE_DIR)/04_move-down/[!readme.txt]*

# check_test: search in test folder tests that above start position - dont need!
check_test:
	for f in $$(find $(TEST_DIR)/$(CASE_DIR) -type f -not -name readme.txt); \
    do awk -f $(TEST_DIR)/check_test.awk $$f; \
    done