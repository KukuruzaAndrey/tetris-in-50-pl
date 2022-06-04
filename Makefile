.PHONY: test

##### DEFINES #####

CFLAGS := -Wall -ggdb3 -std=c11

### DIRS ###
SRCS_DIR := srcs
TEST_DIR := test
CASE_DIR := cases

### BINS ###
RUNNER := runner
TEST_RUNNERS := $(TEST_DIR)/test $(TEST_DIR)/test_init

### PL-S ###
C := ./$(SRCS_DIR)/c/core
JS := ./$(SRCS_DIR)/javascript/core.js
PL ?= C
PL_LIST := C JS


##### RULES #####
all: $(RUNNER) one-runner

$(RUNNER): runner.c utils.c
one-runner: one-runner.c utils.c

c: $(RUNNER) $(SRCS_DIR)/c
	$(CC) $(CFLAGS) -o $(C) $(addsuffix .c,$(C))
	./$(RUNNER) $(C)

js: $(RUNNER)
	./$(RUNNER) $(JS)

$(TEST_RUNNERS): %: %.c utils.c
test: $(TEST_RUNNERS)
	# generate tests for rotate counter-clockwise from rotate clockwise tests 
	# for figures that have two or one rotations (I, S, Z, O) (results are the same, only command is diffirent)
	# find lines with arguments; change first '3' to '4'; write changes in-place (echo do the trick)
	for d in 0_I 3_S 4_Z 5_O; \
	do \
		cp -r $(TEST_DIR)/$(CASE_DIR)/04_rotate-clockwise/$$d $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise; \
		for f in $$(find $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/$$d -type f); \
        do \
			echo "$$(awk '{if (((NR - 1) % 25 == 0) || ((NR - 2) % 25 == 0)) $$1=4; print $$0}' $$f)" > $$f; \
        done \
	done

	$(TEST_DIR)/test_init $(TEST_DIR)/initCases/er.txt $($(PL)) && $(TEST_DIR)/test $(TEST_DIR)/$(CASE_DIR)/$(TEST_PATH) $($(PL))

test_all: $(TEST_RUNNERS)
	$(foreach PL, $(PL_LIST), $(MAKE) test PL=$(PL) &&) true 

one: one-runner
	> logs.txt
	xargs -a ./one-cases.txt -L 1 ./one-runner $($(PL))

clean:
	rm -f $(RUNNER)
	rm -f one-runner
	rm -f ./test/test
	rm -f ./test/test_init
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/0_I
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/3_S
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/4_Z
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/5_O

check_test:
	# exclude game over files
	for f in $$(find $(TEST_DIR)/$(CASE_DIR) -type f ! -name '*#*'); \
	do awk -f $(TEST_DIR)/check_test.awk $$f; \
	done
	
change_tests:
	for f in $$(find $(TEST_DIR)/$(CASE_DIR)/06_drop -type f); \
	do \
		echo "$$(awk '((NR - 2) % 25 == 0) {$$8="?";$$9="?"} {print $$0}' $$f)" > $$f; \
	done