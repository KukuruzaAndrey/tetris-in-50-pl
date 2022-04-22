#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>

#define FRAME_BUFFER_SIZE 1000
#define FRAME_LINES 22
#define ARGS_SIZE 230

#define RED "\033[31m"
#define GREEN "\033[32m"
#define YELLOW "\033[33m"
#define RESET "\x1B[m"

FILE *corePipe;
FILE *testFile;
char actualNextStepResult[ARGS_SIZE];
char expectedNextStepResult[ARGS_SIZE];
char actualRenderResult[FRAME_BUFFER_SIZE];
char expectedRenderResult[FRAME_BUFFER_SIZE];
char line[255];
char *corePath = "../srcs/javascript/core.js";
char coreInputs[ARGS_SIZE];
char coreArgs[ARGS_SIZE + 30];
char *bucket;

int (*pstrcmp)(const char *, const char *);

void *checkError(const void *ptr, const char *description) {
  if (ptr == NULL) {
    perror(description);
    exit(1);
  }

  return (void *) ptr;
}

void compareResults(const char *actual, const char *expected, const char *caseName) {
  int result = strcmp(actual, expected);
  if (result == 0) {
    printf("%s - %sPassed%s\n", caseName, GREEN, RESET);
  } else {
    printf("%s - %sFailed%s\n", caseName, RED, RESET);
    printf("strlen(actualRenderResult) - %lu   strlen(expectedRenderResult) - %lu\n", strlen(actual),
           strlen(expected));
    printf("Actual Result:\n%s\n", actual);
    printf("Expected Result:\n%s\n\n", expected);
  }
}

// compare strings, but when meet '?' in s2 - skip check equality appropriate chars.
// '?' - wildcard for one char
int strcmpWithWildcard(const char *s1, const char *s2) {
  unsigned i;
  for (i = 0; *(s1 + i) && *(s2 + i) && ((*(s2 + i) == '?') || (*(s1 + i) == *(s2 + i))); i++) {}
  return !(*(s1 + i) == *(s2 + i) || *(s2 + i) == '?');
}

// compare strings, but when meet '$' in s2 - skip check equality all next chars up to '\n' in s1.
// '$' - wildcard for chars to next line
int strcmpWithSkip(const char *s1, const char *s2) {
  unsigned i;
  unsigned j;
  unsigned skip;
  for (i = 0, j = 0, skip = 0; *(s1 + i) && *(s2 + j);) {
    if (skip) {
      if (*(s1 + i) == '\n') {
        skip = 0;
      } else {
        i++;
      }
      continue;
    }

    if (*(s2 + j) == '$') {
      skip = 1;
      j++;
      continue;
    }

    if (*(s1 + i) == *(s2 + j)) {
      i++;
      j++;
    } else {
      break;
    }

  }

  return !(*(s1 + i) == *(s2 + j));
}

void run(const char *testFileName) {
  const unsigned useWildcard = strchr(testFileName, '$') != NULL;
  // set default comparator
  pstrcmp = strcmp;

  // open file with test cases
  testFile = checkError(fopen(testFileName, "r"), testFileName);

  // read arguments for test
  while (fgets(coreInputs, ARGS_SIZE, testFile) != NULL) {

    // concatenate path for core and args
    snprintf(coreArgs, sizeof(coreArgs), "%s %s", corePath, coreInputs);

    // open core with args
    corePipe = checkError(popen(coreArgs, "r"), coreArgs);

    // read lines from core - actual result of next inputs
    fgets(actualNextStepResult, ARGS_SIZE, corePipe);

    // read lines from core - actual result of render
    bucket = actualRenderResult;
    while (fgets(line, sizeof(line), corePipe) != NULL) {
      bucket = stpcpy(bucket, line);
    }

    // read lines from case - expected result of next inputs
    fgets(expectedNextStepResult, ARGS_SIZE, testFile);

    // read lines from case - expected result of render
    bucket = expectedRenderResult;
    for (int j = 0; j < FRAME_LINES; ++j) {
      fgets(line, sizeof(line), testFile);
      bucket = stpcpy(bucket, line);
    }

    // if testcase with wildcards - use appropriate compare func 
    if (useWildcard) {
      pstrcmp = strcmpWithWildcard;
    }
    int result = pstrcmp(actualNextStepResult, expectedNextStepResult);
    if (result == 0) {
//      printf("%s - %sPassed%s\n", testFileName, GREEN, RESET);
    } else {
      printf("%s - %sFailed%s\n", testFileName, RED, RESET);
      printf("strlen(actualRenderResult) - %lu   strlen(expectedRenderResult) - %lu\n", strlen(actualNextStepResult),
             strlen(expectedNextStepResult));
      printf("Actual Result:\n%s\n", actualNextStepResult);
      printf("Expected Result:\n%s\n\n", expectedNextStepResult);
      exit(0);
    }
    if (useWildcard) {
      pstrcmp = strcmpWithSkip;
    }
    result = pstrcmp(actualRenderResult, expectedRenderResult);
    if (result == 0) {
      printf("%s - %sPassed%s\n", testFileName, GREEN, RESET);
      printf("%s\n", actualRenderResult);
    } else {
      printf("%s - %sFailed%s\n", testFileName, RED, RESET);
      printf("strlen(actualRenderResult) - %lu   strlen(expectedRenderResult) - %lu\n", strlen(actualRenderResult),
             strlen(expectedRenderResult));
      printf("Actual Result:\n%s\n", actualRenderResult);
      printf("Expected Result:\n%s\n\n", expectedRenderResult);
      printf("%s%s%s\n", RED, "FAIL", RESET);
      exit(0);
    }

    // read empty line
    fgets(line, 255, testFile);
    fclose(corePipe);
  }

  fclose(testFile);
}

void traverseAndExec(char *dirPath, void (*exec)(const char *)) {
  // printf("entry %s\n", dirPath);
  DIR *d = checkError(opendir(dirPath), dirPath);
  struct dirent *entry;
  errno = 0;
  char pathToSubDir[1000];
  while ((entry = readdir(d)) != NULL) {
    snprintf(pathToSubDir, sizeof(pathToSubDir), "%s/%s", dirPath, entry->d_name);
    switch (entry->d_type) {
      case DT_REG:
//        printf("%s%s%s\n", YELLOW, pathToSubDir, RESET);
        run(pathToSubDir);
        break;
      case DT_DIR:
        if (strcmp(entry->d_name, ".") != 0 && strcmp(entry->d_name, "..") != 0) {
          traverseAndExec(pathToSubDir, exec);
        }
        break;
      default:
        break;
    }
  }

  if (errno != 0) {
    perror(dirPath);
  }

  closedir(d);
//  printf("leave %s\n", dirPath);
}

int isDirectory(const char *path) {
  struct stat statbuf;
  if (stat(path, &statbuf) != 0)
    return 0;
  return S_ISDIR(statbuf.st_mode);
}

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("%s\n", "provide arguments");
    return 1;
  }

  if (isDirectory(argv[1])) {
    traverseAndExec(argv[1], &run);
  } else {
    run(argv[1]);
  }
  printf("%s%s%s\n", GREEN, "SUCCESS", RESET);
  return EXIT_SUCCESS;
}
