.PHONY: test test_all

##### DEFINES #####

CFLAGS := -Wall -ggdb3 -std=c11

### DIRS ###
SRCS_DIR := srcs
TEST_DIR := test
CASE_DIR := cases

### BINS ###
RUNNER := runner
TEST_RUNNERS := $(TEST_DIR)/test $(TEST_DIR)/test_init

### PL-S ###  <-- please fill it when add new PL
C := ./$(SRCS_DIR)/c/core
JS := ./$(SRCS_DIR)/js/core.js
JAVA := ./$(SRCS_DIR)/java/core.class
PL ?= JAVA
PL_LIST := C JS JAVA
PL_CMPL_LIST := C JAVA
PL_INTRP_LIST := JS JAVA

INTRP_JS := "node $(JS)"
INTRP_JAVA := "java -cp $(dir $(JAVA)) $(notdir $(JAVA:.class=))"

##### RULES #####
all: run

### ALL PL-S ###
cmpl_all: $(foreach PL, $(PL_CMPL_LIST), $($(PL)))

TEST_PL_LIST ?= PL_LIST
test_all: $(TEST_RUNNERS) cmpl_all
	$(foreach PL, $(TEST_PL_LIST), $(MAKE) test PL=$(PL) &&) true

### RUNNNER ###
$(RUNNER): runner.c utils.c
one-runner: one-runner.c utils.c

### CURRENT PL ###
cmpl: $(if $(filter $(PL), $(PL_CMPL_LIST)), $($(PL)))

# command to exec core at currnt PL
run_$(PL) := $(if $(filter $(PL), $(PL_INTRP_LIST)), $(INTRP_$(PL)), $($(PL)))
run: $(RUNNER) cmpl 
	./$(RUNNER) $(run_$(PL))

### COMPILE PL ### <-- please add new rule when add new compiled PL 
$(C): $(addsuffix .c,$(C)) $(addsuffix .h,$(C))
	$(CC) $(CFLAGS) -o $(C) $(addsuffix .c,$(C))

$(JAVA): $(JAVA:.class=.java)
	javac -Xlint:unchecked $(JAVA:.class=.java)

### TEST ###
$(TEST_RUNNERS): %: %.c utils.c
test: $(TEST_RUNNERS) cmpl
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

	$(TEST_DIR)/test_init $(TEST_DIR)/initCases/er.txt $(run_$(PL)) && $(TEST_DIR)/test $(TEST_DIR)/$(CASE_DIR)/$(TEST_PATH) $(run_$(PL))

### CLEAN ###
clean_C:
	rm -f $(C)

clean_JAVA:
	rm -f $(dir $(JAVA))/*.class

clean: $(foreach PL, $(PL_CMPL_LIST), clean_$(PL))
	rm -f $(RUNNER)
	rm -f one-runner
	rm -f ./test/test
	rm -f ./test/test_init
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/0_I
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/3_S
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/4_Z
	rm -rf $(TEST_DIR)/$(CASE_DIR)/05_rotate-counter-clockwise/5_O


### OTHER ###
one: one-runner
	> logs.txt
	xargs -a ./one-cases.txt -L 1 ./one-runner $($(PL))

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
