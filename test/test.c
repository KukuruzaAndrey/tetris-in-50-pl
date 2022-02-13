#define _GNU_SOURCE

#include "stdio.h"
#include "string.h"
#include "stdlib.h"

#define FRAME_BUFFER_SIZE 1000
#define FRAME_LINES 22
#define ARGS_SIZE 230

FILE *corePipe;
FILE *testFile;
char *testFileName = "./tests.txt";
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
  testFile = checkError(fopen(testFileName, "r"), testFileName);
  fgets(coreInputs, FRAME_BUFFER_SIZE, testFile);
  snprintf(coreArgs, sizeof(coreArgs), "%s %s", corePath, coreInputs);

  printf("%lu %s\n", sizeof coreArgs, coreArgs);
  corePipe = checkError(popen(coreArgs, "r"), coreArgs);

  if (corePipe == NULL) {
    perror("a");
    exit(1);
  }

  unsigned first = 0;
  bucket = actualResult;
  while (fgets(line, sizeof(line), corePipe) != NULL) {
    if (first == 0) {
      first = 1;
    } else {
      bucket = stpcpy(bucket, line);
    }
  }

  // skip next state
//  fgets(line, 1255, testFile);
  bucket = expectedResult;

  for (int j = 0; j < FRAME_LINES; ++j) {
    fgets(line, 1255, testFile);
    bucket = stpcpy(bucket, line);
  }


  printf("%s\n\n", actualResult);
  printf("%s\n\n", expectedResult);
  printf("%lu %lu\n", strlen(actualResult), strlen(expectedResult));
  printf("%d", strcmp(actualResult, expectedResult));

  fclose(corePipe);
  fclose(testFile);
}

int main(int argc, char **argv) {
  eval();
  return EXIT_SUCCESS;
}
