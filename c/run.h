const char* backColors[] = {
  " ", // for empty
  "\033[41m", // BackgroundRed
  "\033[42m", // BackgroundGreen
  "\033[43m", // BackgroundYellow
  "\033[44m", // BackgroundBlue
  "\033[45m", // BackgroundMagenta
  "\033[46m", // BackgroundCyan
  "\033[47m", // BackgroundWhite
};
const int boardW = 10;
const int boardH = 20;
const int scores[] = {10, 30, 60, 100};
enum moves {
  tick = 0, left, right, down, rotateClockwise, rotateCounterClockwise
};
struct figure {
    int squares[4][2];
    int h;
    int w;
    int ofx;
    int ofy;
};
const struct figure figures[7][4] = {
  {
    { .squares = {{0, 0}, {1, 0}, {2, 0}, {3, 0}}, .h = 1, .w = 4, .ofx = 0, .ofy = 2 },
    { .squares = {{0, 0}, {0, 1}, {0, 2}, {0, 3}}, .h = 4, .w = 1, .ofx = 2, .ofy = 0 },
  }, // I
  {
    { .squares = {{0, 0}, {1, 0}, {2, 0}, {0, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
    { .squares = {{0, 0}, {1, 0}, {1, 1}, {1, 2}}, .h = 3, .w = 2, .ofx = 0, .ofy = 0 },
    { .squares = {{0, 1}, {1, 1}, {2, 1}, {2, 0}}, .h = 2, .w = 3, .ofx = 0, .ofy = 0 },
    { .squares = {{0, 0}, {0, 1}, {0, 2}, {1, 2}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
  }, // L
  {
    { .squares = {{0, 0}, {1, 0}, {2, 0}, {2, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
    { .squares = {{1, 0}, {1, 1}, {1, 2}, {0, 2}}, .h = 3, .w = 2, .ofx = 0, .ofy = 0 },
    { .squares = {{0, 0}, {0, 1}, {1, 1}, {2, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 0 },
    { .squares = {{0, 0}, {1, 0}, {0, 1}, {0, 2}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
  }, // Г
  {
    { .squares = {{1, 0}, {2, 0}, {0, 1}, {1, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
    { .squares = {{0, 0}, {0, 1}, {1, 1}, {1, 2}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
  }, // S
  {
    { .squares = {{0, 0}, {1, 0}, {1, 1}, {2, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
    { .squares = {{0, 1}, {1, 0}, {1, 1}, {0, 2}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
  }, // Z
  {
    { .squares = {{0, 0}, {0, 1}, {1, 1}, {1, 0}}, .h = 2, .w = 2, .ofx = 0, .ofy = 0 },
  }, // O
  {
    { .squares = {{0, 0}, {1, 0}, {2, 0}, {1, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 1 },
    { .squares = {{1, 0}, {0, 1}, {1, 1}, {1, 2}}, .h = 3, .w = 2, .ofx = 0, .ofy = 0 },
    { .squares = {{1, 0}, {0, 1}, {1, 1}, {2, 1}}, .h = 2, .w = 3, .ofx = 0, .ofy = 0 },
    { .squares = {{0, 0}, {0, 1}, {0, 2}, {1, 1}}, .h = 3, .w = 2, .ofx = 1, .ofy = 0 },
  }  // T
};
