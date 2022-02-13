#define _GNU_SOURCE
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
  state->move = tick;
  memset(state->board, 0, sizeof(state->board));
  state->figIndex = randomIntInclusive(0, FIG_COUNT - 1);;
  state->rotateIndex = 0;
  state->color = randomIntInclusive(1, COLORS_COUNT - 1);
  state->offsetX = state->figIndex == 0 ? 3 : 4;
  state->offsetY = -1 * (int) figures[state->figIndex].rotations[0/*rotateIndex*/].ofy - 1;
  state->nextFigIndex = randomIntInclusive(0, FIG_COUNT - 1);
  state->nextFigColor = randomIntInclusive(1, COLORS_COUNT - 1);
  state->score = 0;
}

void printState(const struct state *state) {
  char strBoard[BOARD_H * BOARD_W + 1];
  for (unsigned y = 0; y < BOARD_H; ++y) {
    for (unsigned x = 0; x < BOARD_W; ++x) {
      strBoard[y * BOARD_W + x] = (char) ((unsigned) '0' + state->board[y][x]);
    }
  }
  strBoard[BOARD_H * BOARD_W] = 0;
  printf("%u %s %u %u %u %d %d %u %u %u\n",
         state->move, strBoard, state->figIndex, state->rotateIndex, state->color,
         state->offsetX, state->offsetY, state->nextFigIndex, state->nextFigColor, state->score);
}

void parseState(char **argv, struct state *dest) {
  dest->move = (enum move) (atoi(argv[1]));
  for (unsigned i = 0; i < BOARD_H * BOARD_W; ++i) {
    unsigned y = i / BOARD_W;
    unsigned x = i % BOARD_W;
    dest->board[y][x] = argv[2][i] - '0';
  }
  dest->figIndex = atoi(argv[3]);
  dest->rotateIndex = atoi(argv[4]);
  dest->color = atoi(argv[5]);
  dest->offsetX = atoi(argv[6]);
  dest->offsetY = atoi(argv[7]);
  dest->nextFigIndex = atoi(argv[8]);
  dest->nextFigColor = atoi(argv[9]);
  dest->score = atoi(argv[10]);
}

void calcFigCoords(struct coords *coords, unsigned figIndex, unsigned rotateIndex, int offsetX, int offsetY) {
  unsigned count = 0;
  for (unsigned i = 0; i < 4; ++i) {
    // check overflow !
    unsigned y = figures[figIndex].rotations[rotateIndex].squares[count][1] + offsetY +
            figures[figIndex].rotations[rotateIndex].ofy;
    unsigned x = figures[figIndex].rotations[rotateIndex].squares[count][0] + offsetX +
            figures[figIndex].rotations[rotateIndex].ofx;
    if (y > 20)
      continue;

    coords->squares[count][0] = x;
    coords->squares[count][1] = y;
    count += 1;
  }
  coords->count = count;
}

void update(struct state *state) {
  struct coords oldCoords;
  calcFigCoords(&oldCoords, state->figIndex, state->rotateIndex, state->offsetX, state->offsetY);

  unsigned newRotateIndex = state->rotateIndex;
  int newOffsetX = state->offsetX;
  int newOffsetY = state->offsetY;

  // update piece position
  switch (state->move) {
    case tick:
    case down:
      newOffsetY += 1;
      break;
    case left:
      if (state->offsetX + figures[state->figIndex].rotations[state->rotateIndex].ofx <= 0)
        break;
      unsigned left_blocked = 0;
      for (unsigned i = 0; i < oldCoords.count; ++i) {
        unsigned x = oldCoords.squares[i][0];
        unsigned y = oldCoords.squares[i][1];
        if (state->board[y][x - 1] != 0) {
          left_blocked = 1;
          break;
        }
      }
      if (!left_blocked) {
        newOffsetX -= 1;
      }
      break;
    case right:
      if (state->offsetX + figures[state->figIndex].rotations[state->rotateIndex].w +
          figures[state->figIndex].rotations[state->rotateIndex].ofx >= BOARD_W)
        break;
      unsigned right_blocked = 0;
      for (unsigned i = 0; i < oldCoords.count; ++i) {
        unsigned x = oldCoords.squares[i][0];
        unsigned y = oldCoords.squares[i][1];
        if (state->board[y][x + 1] != 0) {
          right_blocked = 1;
          break;
        }
      }
      if (!right_blocked) {
        newOffsetX += 1;
      }
      break;
    case rotateClockwise: {
      unsigned possibleRotateIndex = (state->rotateIndex == figures[state->figIndex].count - 1) ? 0 :
                                     state->rotateIndex + 1;
//      struct coords rotateFigCoords;
//      calcFigCoords(&rotateFigCoords, state->figIndex, possibleRotateIndex, state->offsetX, state->offsetY);

      unsigned rotate_clockwise_blocked = 0;
      for (unsigned i = 0; i < 4; ++i) {
        // check overflow !
        unsigned y = figures[state->figIndex].rotations[rotateIndex].squares[count][1] + offsetY +
                     figures[state->figIndex].rotations[rotateIndex].ofy;
        unsigned x = figures[state->figIndex].rotations[rotateIndex].squares[count][0] + offsetX +
                     figures[state->figIndex].rotations[rotateIndex].ofx;
        if (y > 20)
          continue;

        coords->squares[count][0] = x;
        coords->squares[count][1] = y;
        count += 1;
      }


      for (unsigned i = 0; i < rotateFigCoords.count; ++i) {
        unsigned x = rotateFigCoords.squares[i][0];
        unsigned y = rotateFigCoords.squares[i][1];
        if (x < 0 || x >= BOARD_W || state->board[y][x] != 0) {
          rotate_clockwise_blocked = 1;
          break;
        }
      }
      if (!rotate_clockwise_blocked) {
        newRotateIndex = possibleRotateIndex;
      }
    }
      break;
    case rotateCounterClockwise: {
      unsigned possibleRotateIndex = (state->rotateIndex == 0) ? figures[state->figIndex].count - 1 :
                                     state->rotateIndex - 1;
      struct coords rotateFigCoords;
      calcFigCoords(&rotateFigCoords, state->figIndex, possibleRotateIndex, state->offsetX, state->offsetY);
      unsigned rotate_counter_clockwise_blocked = 0;
      for (unsigned i = 0; i < oldCoords.count; ++i) {
        unsigned x = oldCoords.squares[i][0];
        unsigned y = oldCoords.squares[i][1];
        if (x < 0 || x >= BOARD_W || state->board[y][x] != 0) {
          rotate_counter_clockwise_blocked = 1;
          break;
        }
      }
      if (!rotate_counter_clockwise_blocked) {
        newRotateIndex = possibleRotateIndex;
      }
    }
      break;
  }

  // calculate new coordinates
  struct coords newCoords;
  calcFigCoords(&newCoords, state->figIndex, newRotateIndex, newOffsetX, newOffsetY);

  // check is new position is overlap or on floor
  unsigned overlap = 0;
  for (unsigned i = 0; i < newCoords.count; ++i) {
    unsigned x = newCoords.squares[i][0];
    unsigned y = newCoords.squares[i][1];
    if (y == BOARD_H || state->board[y][x] != 0) {
      overlap = 1;
      break;
    }
  }

  if (overlap) {
    // paint piece back
    for (unsigned i = 0; i < oldCoords.count; ++i) {
      unsigned x = oldCoords.squares[i][0];
      unsigned y = oldCoords.squares[i][1];
      state->board[y][x] = state->color;
    }

    // remove full lines
    unsigned countFullLines = 0;
    for (unsigned y = 0; y < BOARD_H; ++y) {
      unsigned full = 1;
      for (unsigned x = 0; x < BOARD_W; ++x) {
        if (state->board[y][x] == 0) {
          full = 0;
          break;
        }
      }
      if (full) {
        memmove(state->board[y], state->board[countFullLines], (y - countFullLines) * (sizeof state->board[y]));
        countFullLines++;
      }
    }

    if (countFullLines > 0) {
      // update score
      state->score += scores[countFullLines - 1];

      // add new empty lines
      memset(state->board[0], 0, countFullLines * (sizeof state->board[0]));
    }

    // create new piece
    state->figIndex = state->nextFigIndex;
    state->nextFigIndex = randomIntInclusive(0, FIG_COUNT - 1);
    state->rotateIndex = 0;
    state->offsetX = state->figIndex == 0 ? 3 : 4;
    state->offsetY = -1 * (int) figures[state->figIndex].rotations[0/*rotateIndex*/].ofy - 1;
    state->color = state->nextFigColor;
    state->nextFigColor = randomIntInclusive(1, COLORS_COUNT - 1);

    struct coords newCoords;
    calcFigCoords(&newCoords, state->figIndex, state->rotateIndex, state->offsetX, state->offsetY);

    // check end of game
    unsigned end = 0;
    for (unsigned i = 0; i < newCoords.count; ++i) {
      unsigned x = newCoords.squares[i][0];
      unsigned y = newCoords.squares[i][1];
      if (state->board[y][x] != 0) {
        end = 1;
        break;
      }
    }
    if (end) {
      printf("%s", "The End");
      exit(0);
    }
  } else {
    state->rotateIndex = newRotateIndex;
    state->offsetX = newOffsetX;
    state->offsetY = newOffsetY;
  }
}

void render(char *res, struct state *state) {

  // add piece to board for simplifying render
  struct coords coords;
  calcFigCoords(&coords, state->figIndex, state->rotateIndex, state->offsetX, state->offsetY);
  for (unsigned i = 0; i < coords.count; ++i) {
    unsigned x = coords.squares[i][0];
    unsigned y = coords.squares[i][1];
    state->board[y][x] = state->color;
  }

  char *bucket = stpcpy(res, " ");
  for (unsigned x = 0; x < BOARD_W; x++) {
    bucket = stpcpy(bucket, Ceil);
  }
  bucket = stpcpy(bucket, " \n");

  for (unsigned y = 0; y < BOARD_H; y++) {
    bucket = stpcpy(bucket, Left);
    for (unsigned x = 0; x < BOARD_W; x++) {
      if (state->board[y][x] != 0) {
        bucket = stpcpy(bucket, backColors[state->board[y][x]]);
        bucket = stpcpy(bucket, " ");
        bucket = stpcpy(bucket, Reset);
      } else {
        bucket = stpcpy(bucket, (x % 2 == 0) ? " " : spacer);
      }
    }
    bucket = stpcpy(bucket, Right);

//    if (y == = 0) {
//      res += ' ' + String(score).padStart(6, '0')
//    }
//
//    if (y > 0 && y - 1 < nP.length) {
//      res += nP[y - 1]
//    }

    bucket = stpcpy(bucket, "\n");
  }

  bucket = stpcpy(bucket, " ");
  for (unsigned x = 0; x < BOARD_W; x++) {
    bucket = stpcpy(bucket, Floor);
  }
  bucket = stpcpy(bucket, " ");

  for (unsigned i = 0; i < coords.count; ++i) {
    unsigned x = coords.squares[i][0];
    unsigned y = coords.squares[i][1];
    state->board[y][x] = 0;
  }

}

int main(int argc, char **argv) {
  struct state state;

  if (strcmp(argv[1], "0init") == 0) {
    initState(&state);
    printState(&state);
  } else if (argc == 11) {
    parseState(argv, &state);
    update(&state);
    printState(&state);
    printState(&state);
    char res[1500];
    render(res, &state);
    printf("%s\n", res);
  } else {
    puts("incorrect arguments");
    exit(1);
  }

  return 0;
}

