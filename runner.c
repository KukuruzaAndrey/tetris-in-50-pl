#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>

#define FRAME_TIME_MS (500)
#define FRAME_TIME_NS (FRAME_TIME_MS * 1000 * 1000)

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

//int8_t getch() {
//    int r;
//    unsigned char c;
//    if ((r = read(STDIN_FILENO, &c, sizeof(c))) < 0) {
//        return r;
//    } else {
//        return c;
//    }
//}

FILE *fp;
char path[1035];
char cmdBuff[1035];
char arg[200] = "3 4 4 6 2 7 2 8 2";
char *cmd = "./game-core.js";

struct termios original;

#define CTRL_KEY(k) ((k) & 0x1f)

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
//                1: "left", 2: "down", 3: "right", 4: "up",
                switch (n) {
                    case 'D':
                        arg[0] = '1';
                        break;
                    case 'A':
                        arg[0] = '4';
                        break;
                    case 'C':
                        arg[0] = '3';
                        break;
                    case 'B':
                        arg[0] = '2';
                        break;
                }
            }

    }
}

void eval() {
    snprintf(cmdBuff, sizeof(cmdBuff), "%s %s", cmd, arg);
    fp = popen(cmdBuff, "r");
    unsigned i = 0;
    while (fgets(path, sizeof(path), fp) != NULL) {
        if (i == 0) {
            strncpy(arg, path, sizeof(arg));
            i++;
        } else {
            printf("%s", path);
        }
    }
}

int main(void) {
    enableRawMode();
    uint32_t nanos;
    uint32_t last_nanos;
    nanos = get_nanos();
    last_nanos = nanos;
    while (1) {
        nanos = get_nanos();
        if (kbhit()) {
            editorProcessKeypress(arg);
            eval();
            last_nanos = nanos;
        } else if (nanos - last_nanos > FRAME_TIME_NS) {
//            printf("%ld", (nanos - last_nanos) / 1000000);
            eval();
            last_nanos = nanos;
        }
    }
    return EXIT_SUCCESS;
}
