#include <stdlib.h>
#include <stdio.h>

void *checkError(const void *ptr, const char *description) {
  if (ptr == NULL) {
    perror(description);
    exit(1);
  }

  return (void *) ptr;
}