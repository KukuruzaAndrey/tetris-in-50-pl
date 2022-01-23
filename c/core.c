#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "core.h"

unsigned randomInt(void) {
  unsigned rand;
  FILE *fp;
  fp = fopen("/dev/urandom", "r");
  fread(&rand, sizeof(unsigned), 1, fp);
  fclose(fp);
  return rand;
}

unsigned randomIntInclusive(unsigned min, unsigned max) {
  return (randomInt() % (max - min + 1)) + min;
}

void initState(struct state *state) {
  unsigned figIndex = randomIntInclusive(0, FIG_COUNT - 1);
  state->move = tick;
  memset(state->board, 0, sizeof(state->board));
  state->figIndex = figIndex;
  state->rotateIndex = 0;
  state->color = randomIntInclusive(1, COLORS_COUNT - 1);
  state->offsetX = figIndex == 0 ? 3 : 4;
  state->offsetY = -1 * figures[figIndex][0/*rotateIndex*/].ofy - 1;
  state->nextFigIndex = randomIntInclusive(0, FIG_COUNT - 1);
  state->nextFigColor = randomIntInclusive(1, COLORS_COUNT - 1);
  state->score = 0;
}

void printState(const struct state *state) {
  char strBoard[BOARD_H * BOARD_W + 1];
  for (int y = 0; y < BOARD_H; ++y) {
    for (int x = 0; x < BOARD_W; ++x) {
      strBoard[y * BOARD_W + x] = '0' + state->board[y][x];
    }
  }
  strBoard[BOARD_H * BOARD_W] = 0;
  printf("%u %s %u %u %u %u %d %u %u %u\n",
         state->move, strBoard, state->figIndex, state->rotateIndex, state->color,
         state->offsetX, state->offsetY, state->nextFigIndex, state->nextFigColor, state->score);
}

void parseState(char **argv, struct state *dest) {
  dest->move = (enum move) (*argv[1] - '0');
  for (int i = 0; i < BOARD_H * BOARD_W; ++i) {
    unsigned y = i / 10;
    unsigned x = i % 10;
    dest->board[y][x] = argv[2][i] - '0';
  }
  dest->figIndex = *argv[3] - '0';
  dest->rotateIndex = *argv[4] - '0';
  dest->color = *argv[5] - '0';
  dest->offsetX = *argv[6] - '0';
  dest->offsetY = *argv[7] - '0';
  dest->nextFigIndex = *argv[8] - '0';
  dest->nextFigColor = *argv[9] - '0';
  dest->score = *argv[10] - '0';
}


int main(int argc, char **argv) {
  struct state state;
  if (strcmp(argv[1], "0init") == 0) {
    initState(&state);
    printState(&state);
  } else if (argc == 11) {
    parseState(argv, &state);
    printState(&state);
  } else {
    puts("incorrect arguments");
    exit(1);
  }

  return 0;
}

