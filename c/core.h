#define BOARD_W 10
#define BOARD_H 20

#define FIG_COUNT 7
#define COLORS_COUNT 8

struct coords {
  unsigned count;
  unsigned squares[4][2];
};

struct rotation {
  unsigned squares[4][2];
  unsigned h;
  unsigned w;
  unsigned ofx;
  unsigned ofy;
};

struct figure {
  unsigned count;
  struct rotation rotations[4];
};
enum move {
  tick = 0,
  left,
  right,
  down,
  rotateClockwise,
  rotateCounterClockwise
};

struct state {
  enum move move;
  unsigned board[BOARD_H][BOARD_W];
  unsigned figIndex;
  unsigned rotateIndex;
  unsigned color;
  unsigned offsetX;
  int offsetY;
  unsigned nextFigIndex;
  unsigned nextFigColor;
  unsigned score;
};
const int scores[] = {10, 30, 60, 100};

const char *backColors[] = {
  " ", // for empty
  "\033[41m", // BackgroundRed
  "\033[42m", // BackgroundGreen
  "\033[43m", // BackgroundYellow
  "\033[44m", // BackgroundBlue
  "\033[45m", // BackgroundMagenta
  "\033[46m", // BackgroundCyan
  "\033[47m", // BackgroundWhite
};

const struct figure figures[FIG_COUNT] = {
  {
    .count = 2,
    .rotations = {
      { .squares = {{0, 0}, {1, 0}, {2, 0}, {3, 0}}, .h = 1, .w = 4, .ofx = 0, .ofy = 2 },
      { .squares = {{0, 0}, {0, 1}, {0, 2}, {0, 3}}, .h = 4, .w = 1, .ofx = 2, .ofy = 0 },
    }
  }, // I
  {
    .count = 4,
    .rotations = {
      { .squares = {{0, 0}, {1, 0}, {1, 1}, {1, 2}}, .h = 3, .w = 2, .ofx = 0, .ofy = 0 },
      { .squares = {{0, 0}, {1, 0}, {2, 0}, {0, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
      { .squares = {{0, 1}, {1, 1}, {2, 1}, {2, 0}}, .h = 2, .w = 3, .ofx = 0, .ofy = 0 },
      { .squares = {{0, 0}, {0, 1}, {0, 2}, {1, 2}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
    }
  }, // L
  {
    .count = 4,
    .rotations = {
      { .squares = {{0, 0}, {1, 0}, {2, 0}, {2, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
      { .squares = {{1, 0}, {1, 1}, {1, 2}, {0, 2}}, .h = 3, .w = 2, .ofx = 0, .ofy = 0 },
      { .squares = {{0, 0}, {0, 1}, {1, 1}, {2, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 0 },
      { .squares = {{0, 0}, {1, 0}, {0, 1}, {0, 2}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
    }
  }, // Г
  {
    .count = 2,
    .rotations = {
      { .squares = {{1, 0}, {2, 0}, {0, 1}, {1, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
      { .squares = {{0, 0}, {0, 1}, {1, 1}, {1, 2}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
    }
  }, // S
  {
    .count = 2,
    .rotations = {
      { .squares = {{0, 0}, {1, 0}, {1, 1}, {2, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
      { .squares = {{0, 1}, {1, 0}, {1, 1}, {0, 2}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
    }
  }, // Z
  {
    .count = 1,
    .rotations = {
      { .squares = {{0, 0}, {0, 1}, {1, 1}, {1, 0}}, .h = 2, .w = 2, .ofx = 0, .ofy = 0 },
    }
  }, // O
  {
    .count = 2,
    .rotations = {
      { .squares = {{0, 0}, {1, 0}, {2, 0}, {1, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
      { .squares = {{1, 0}, {0, 1}, {1, 1}, {1, 2}}, .h = 3, .w = 2, .ofx = 0, .ofy = 0 },
      { .squares = {{1, 0}, {0, 1}, {1, 1}, {2, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 0 },
      { .squares = {{0, 0}, {0, 1}, {0, 2}, {1, 1}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
    }
  }  // T
};

#define Reset "\x1B[m"
#define Inverse "\x1B[7m"
#define Ceil "\u2582"
#define Floor Inverse "\u2586" Reset
#define Left Inverse "\u258a" Reset
#define Right "\u258e"
#define spacer "."
