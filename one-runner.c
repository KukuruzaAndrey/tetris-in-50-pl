#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <bits/types/struct_timeval.h>
#include <sys/select.h>

#define FRAME_TIME_MS (400)
#define FRAME_TIME_NS (FRAME_TIME_MS * 1000 * 1000)
#define CTRL_KEY(k) ((k) & 0x1f)

struct termios original;

void disableRawMode() {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &original);
}

void enableRawMode() {
  tcgetattr(STDIN_FILENO, &original);
  atexit(disableRawMode);

  struct termios raw = original;
  raw.c_iflag &= ~(ICRNL | IXON);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

FILE *fp;
FILE *logs;
char path[1035];
char cmdBuff[1035];
char arg[500] = "0init";
char *cmd;


void eval() {
  snprintf(cmdBuff, sizeof(cmdBuff), "%s %s", cmd, arg);
  fp = popen(cmdBuff, "r");
  fprintf(logs, "%s\n", "\"I\" rotation 0 move left - in front of the wall");
  fputs(arg, logs);

  if (fp == NULL) {
    printf("%s", cmdBuff);
    perror("a");
    exit(1);
  }
  unsigned line = 0;
  while (fgets(path, sizeof(path), fp) != NULL) {
    if (line == 0) {
      strncpy(arg, path, sizeof(arg));
      if (strcmp(path, "The End\n") == 0) {
        fputs("=== END ===\n", logs);
        fclose(logs);
        exit(0);
      }
      fputs(path, logs);
      line = 1;
    } else {
      printf("%s", path);
      fputs(path, logs);
    }
  }
  fputs("\n", logs);
  fclose(fp);
}

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("%s\n", "provide arguments");
    exit(1);
  } else if (argc == 2) {
  } else if (argc == 12) {
    sprintf(arg, "%s %s %s %s %s %s %s %s %s %s\n", argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8],
            argv[9], argv[10], argv[11]);
//    printf("\n--%s--\n", arg);
  } else {
    printf("%s\n", "incorrect number of arguments");
    exit(1);
  }

  cmd = argv[1];
  logs = fopen("logs.txt", "a+");


  enableRawMode();
  eval();
  return EXIT_SUCCESS;
}
