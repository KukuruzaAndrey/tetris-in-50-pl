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

test:
	$(CC) $(CFLAGS) -o $(TEST_DIR)/test $(TEST_DIR)/test.c utils.c
	
	## generate tests for rotate counter-clockwise from rotate clockwise tests 
	# for figures that have two or one rotations (I, S, Z, O) (results are the same, only command is diffirent)
	for f in 0_I 3_S 4_Z 5_O; \
	do \
		cp -r $(TEST_DIR)/$(CASE_DIR)/04_rotate-clockwise/$$f $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise; \
	done
	# for all test files
	# find lines with arguments; change first '3' to '4'; write changes in-place (echo do the trick)
	for d in 0_I 3_S 4_Z 5_O; \
	do \
		for f in $$(find $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/$$d -type f); \
		do \
			echo "$$(awk '{if (((NR - 1) % 25 == 0) || ((NR - 2) % 25 == 0)) $$1=4; print $0}' $$f)" > $$f; \
		done \
	done
	cd $(TEST_DIR) && ./test $(CASE_DIR)

one: one-runner
	> logs.txt
	xargs -a ./one-cases.txt -L 1 ./one-runner ./srcs/javascript/core.js

clean:
	rm -f $(RUNNER)
	rm -f one-runner
	rm -f ./test/test
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/0_I
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/3_S
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/4_Z
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/5_O

check_test:
	# search in test folder for tests that above start position - dont need!
	for f in $$(find $(TEST_DIR)/$(CASE_DIR) -type f); \
	do awk -f $(TEST_DIR)/check_test.awk $$f; \
	done