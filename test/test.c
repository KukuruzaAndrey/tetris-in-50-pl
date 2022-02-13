#define _GNU_SOURCE

#include "stdio.h"
#include "string.h"
#include "stdlib.h"

#define FRAME_BUFFER_SIZE 1000
#define FRAME_LINES 22
#define ARGS_SIZE 230

#define RED "\033[31m"
#define GREEN "\033[32m"
#define RESET "\x1B[m"

FILE *corePipe;
FILE *testFile;
char *testFileName = "./tests.txt";
char testCaseName[255];
char actualResult[FRAME_BUFFER_SIZE];
char expectedResult[FRAME_BUFFER_SIZE];
char line[255];
char *corePath = "../javascript/core.js";
char coreInputs[ARGS_SIZE];
char coreArgs[ARGS_SIZE + 30];
char *bucket;

void *checkError(void *ptr, char *description) {
  if (ptr == NULL) {
    perror(description);
    exit(1);
  }

  return ptr;
}

void eval() {
  // open file with test cases
  testFile = checkError(fopen(testFileName, "r"), testFileName);

  // read name of test case
  while (fgets(testCaseName, 255, testFile) != NULL) {
    // remove trailing \n from name
    testCaseName[strlen(testCaseName) - 1] = 0;

    // read arguments for test
    fgets(coreInputs, ARGS_SIZE, testFile);

    // concatenate path for core and args
    snprintf(coreArgs, sizeof(coreArgs), "%s %s", corePath, coreInputs);

    // open core with args
    corePipe = checkError(popen(coreArgs, "r"), coreArgs);

    // read lines from core - actual result of test
    bucket = actualResult;
    unsigned first = 0;
    while (fgets(line, sizeof(line), corePipe) != NULL) {
      if (first == 0) {
        first = 1;
      } else {
        bucket = stpcpy(bucket, line);
      }
    }

    // read lines from case - expected result of test
    bucket = expectedResult;
    for (int j = 0; j < FRAME_LINES; ++j) {
      fgets(line, 255, testFile);
      bucket = stpcpy(bucket, line);
    }

    int result = strcmp(actualResult, expectedResult);
    if (result == 0) {
      printf("%s - %sPassed%s\n", testCaseName, GREEN, RESET);
    } else {
      printf("%s - %sFailed%s\n", testCaseName, RED, RESET);
      printf("strlen(actualResult) - %lu   strlen(expectedResult) - %lu\n", strlen(actualResult),
             strlen(expectedResult));
      printf("Actual Result:%s\n", actualResult);
      printf("Expected Result:%s\n\n", expectedResult);
    }

    // read empty line
    fgets(line, 255, testFile);
  }
  fclose(corePipe);
  fclose(testFile);
}

int main(int argc, char **argv) {
  eval();
  return EXIT_SUCCESS;
}
