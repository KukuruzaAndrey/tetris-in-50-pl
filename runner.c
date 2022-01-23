#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>

#define FRAME_TIME_MS (400)
#define FRAME_TIME_NS (FRAME_TIME_MS * 1000 * 1000)
#define CTRL_KEY(k) ((k) & 0x1f)

static uint32_t get_nanos(void) {
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    return (long) ts.tv_sec * 1000000000L + ts.tv_nsec;
}

int8_t kbhit() {
    struct timeval tv = {0L, 0L};
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(0, &fds);
    return select(1, &fds, NULL, NULL, &tv) > 0;
}

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
char path[1035];
char cmdBuff[1035];
char arg[500] = "0init";
char *cmd;

void editorProcessKeypress(char *arg) {
    char c, n;
    read(STDIN_FILENO, &c, sizeof(c));
    switch (c) {
        case CTRL_KEY('q'):
            exit(0);
        case '\x1b':
            read(STDIN_FILENO, &n, 1);
            if (n == '[') {
                read(STDIN_FILENO, &n, 1);
//                 tick: 0, left: 1, right: 2, down: 3, rotateClockwise: 4, rotateCounterClockwise: 5,
                switch (n) {
                    case 'B':
                        arg[0] = '3';
                        break;
                    case 'C':
                        arg[0] = '2';
                        break;
                    case 'D':
                        arg[0] = '1';
                        break;
                }
            }
            break;
        case 'z':
            arg[0] = '4';
            break;
        case 'x':
            arg[0] = '5';
            break;

    }
}

void eval() {
    snprintf(cmdBuff, sizeof(cmdBuff), "%s %s", cmd, arg);
    fp = popen(cmdBuff, "r");
    if (fp == NULL) {
        printf("%s", cmdBuff);
        perror("a");
    }
    unsigned i = 0;
    while (fgets(path, sizeof(path), fp) != NULL) {
        if (i == 0) {
            strncpy(arg, path, sizeof(arg));
            if (strcmp(path, "The End\n") == 0) {
                exit(0);
            };

            i = 1;
        } else {
            printf("%s", path);
        }
    }
    fclose(fp);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("%s\n", "provide arguments");
        exit(1);
    }
    cmd = argv[1];
    enableRawMode();
    uint32_t nanos;
    uint32_t last_nanos;
    nanos = get_nanos();
    last_nanos = nanos;
    while (1) {
        nanos = get_nanos();
        if (nanos - last_nanos > FRAME_TIME_NS) {
            arg[0] = '0';
            eval();
            last_nanos = nanos;
        }
        if (kbhit()) {
            editorProcessKeypress(arg);
            eval();
        }
        //last_nanos = nanos;
//          else if (nanos - last_nanos > FRAME_TIME_NS) {
//            printf("%ld", (nanos - last_nanos) / 1000000);

//         }
    }
    return EXIT_SUCCESS;
}
