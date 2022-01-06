#include "stdio.h"

#include "run.h"

int main(int argc, char **argv) {

    for (int i = 0; i < 7; i++) {
        for(int j = 0; j < 4; j++) {
            printf("%d, %d, %d\n", i, j, figures[i][j].h);
        }
    }
    return 0;
}

