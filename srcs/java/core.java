import java.util.Arrays;
import java.util.ArrayList;
import java.util.stream.IntStream;

class core {
  public static void main(String args[]) {
    if (args.length == 10) {
      Game game = new Game(args);
      game.update();
      System.out.println(game.getNextState());
      System.out.println(game.render());
    } else if (args.length == 1 && args[0].equals("INIT_STATE")) {
      Game game = new Game();
      System.out.println(game.getNextState());
      System.out.println(game.render());
    }
  }
}

class Random {
  public static int getRandomIntInclusive(int min, int max) {
    return (int)(Math.random() * (max - min + 1)) + min;
  }
}
class Rotation {
  public int[][] squares;
  public int ofx;
  public int ofy;

  public Rotation(int[][] squares, int ofx, int ofy) {
    this.squares = squares;
    this.ofx = ofx;
    this.ofy = ofy;
  }
}

class Figure {
  public Rotation rotations[];

  public Figure(Rotation rotations[]) {
    this.rotations = rotations;
  }
}

class BoardFigure {
  private static final Figure[] FIGURES = {
    new Figure(new Rotation[] { // I
        new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 3, 0 } }, 0, 2),
        new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 0, 3 } }, 2, 0)
      }),
    new Figure(new Rotation[] { // L
        new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 0, 1 } }, 0, 1),
        new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 1, 1 }, { 1, 2 } }, 0, 0),
        new Rotation(new int[][] { { 0, 1 }, { 1, 1 }, { 2, 1 }, { 2, 0 } }, 0, 0),
        new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 1, 2 } }, 1, 0)
      }),
    new Figure(new Rotation[] { // J
        new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 2, 1 } }, 0, 1),
        new Rotation(new int[][] { { 1, 0 }, { 1, 1 }, { 1, 2 }, { 0, 2 } }, 0, 0),
        new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 2, 1 } }, 0, 0),
        new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 0, 1 }, { 0, 2 } }, 1, 0)
      }),
    new Figure(new Rotation[] { // S
        new Rotation(new int[][] { { 1, 0 }, { 2, 0 }, { 0, 1 }, { 1, 1 } }, 0, 1),
        new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 1, 2 } }, 1, 0)
      }),
    new Figure(new Rotation[] { // Z
        new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 1, 1 }, { 2, 1 } }, 0, 1),
        new Rotation(new int[][] { { 0, 1 }, { 1, 0 }, { 1, 1 }, { 0, 2 } }, 1, 0)
      }),
    new Figure(new Rotation[] { // O
        new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 1, 0 } }, 0, 0)
      }),
    new Figure(new Rotation[] { // T
        new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 1, 1 } }, 0, 1),
        new Rotation(new int[][] { { 1, 0 }, { 0, 1 }, { 1, 1 }, { 1, 2 } }, 0, 0),
        new Rotation(new int[][] { { 1, 0 }, { 0, 1 }, { 1, 1 }, { 2, 1 } }, 0, 0),
        new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 1, 1 } }, 1, 0)
      })
  };

  private static final String[] COLORS = {
    "", // for empty
    "\033[41m", // BackgroundRed
    "\033[42m", // BackgroundGreen
    "\033[43m", // BackgroundYellow
    "\033[44m", // BackgroundBlue
    "\033[45m", // BackgroundMagenta
    "\033[46m", // BackgroundCyan
    "\033[47m" // BackgroundWhite
  };

  private Figure figure;
  private int figIndex;
  private int rotateIndex;
  public int color;
  public int offsetX;
  public int offsetY;

  public static int getNextFigIndex() {
    return Random.getRandomIntInclusive(0, FIGURES.length - 1);
  }

  public static int getNextFigColor() {
    return Random.getRandomIntInclusive(1, COLORS.length - 1);
  }

  public BoardFigure() {
    figIndex = getNextFigIndex();
    rotateIndex = 0;
    color = getNextFigColor();
    figure = FIGURES[figIndex];
    offsetX = figIndex == 0 ? 3 : 4;
    offsetY = -1 * figure.rotations[rotateIndex].ofy;
  }

  public BoardFigure(int figIndex, int rotateIndex, int color, int offsetX, int offsetY) {
    this.figIndex = figIndex;
    this.rotateIndex = rotateIndex;
    this.color = color;
    this.figure = FIGURES[figIndex];
    this.offsetX = offsetX;
    this.offsetY = offsetY;
  }

  public BoardFigure(int figIndex, int color) {
    this.figIndex = figIndex;
    this.rotateIndex = 0;
    this.color = color;
    this.figure = FIGURES[figIndex];
    this.offsetX = figIndex == 0 ? 3 : 4;
    this.offsetY = -1 * figure.rotations[rotateIndex].ofy;
  }

  @Override
  public String toString() {
    return String.valueOf(figIndex) + " " + String.valueOf(rotateIndex) + " " + String.valueOf(color) + " " + String.valueOf(offsetX) +
      " " + String.valueOf(offsetY);
  }

  public int[][] getCoords() {
    int [][] res = new int[4][2];
    for(int i = 0; i < 4; ++i) {
      res[i][0] = figure.rotations[rotateIndex].squares[i][0] + offsetX + figure.rotations[rotateIndex].ofx;
      res[i][1] = figure.rotations[rotateIndex].squares[i][1] + offsetY + figure.rotations[rotateIndex].ofy;
    }
    return res;
  }

  // crutch
  public void moveUp() {
    offsetY -= 1;
  }

  public void moveDown() {
    offsetY += 1;
  }
  public void moveLeft() {
    offsetX -= 1;
  }
  public void moveRight() {
    offsetX += 1;
  }

  public void rotateClockwise() {
    int newIndex = (rotateIndex == FIGURES[figIndex].rotations.length - 1) ? 0 : rotateIndex + 1;
    rotateIndex = newIndex;
  }

  public void rotateCounterClockwise() {
    int newIndex = (rotateIndex == 0) ? FIGURES[figIndex].rotations.length - 1 : rotateIndex - 1;
    rotateIndex = newIndex;
  }

}

class SmallBoard {
  private static final String[] COLORS = {
    "", // for empty
    "\033[41m", // BackgroundRed
    "\033[42m", // BackgroundGreen
    "\033[43m", // BackgroundYellow
    "\033[44m", // BackgroundBlue
    "\033[45m", // BackgroundMagenta
    "\033[46m", // BackgroundCyan
    "\033[47m" // BackgroundWhite
  };

  private static int NEXT_P_BOARD_W = 6;
  private static int NEXT_P_BOARD_H = 6;
  private static String RESET = "\u001B[m";
  private static String INVERSE = "\u001B[7m";
  private static String CEIL = "\u2582";
  private static String FLOOR = INVERSE + "\u2586" + RESET;
  private static String LEFT = INVERSE + "\u258a" + RESET;
  private static String RIGHT = "\u258e";
  
  public int nextFigIndex;
  public int nextFigColor;
  public int score;
  private BoardFigure bf;

  public SmallBoard() {
    this(0);
  }

  public SmallBoard(int score) {
    this(BoardFigure.getNextFigIndex(), BoardFigure.getNextFigColor(), score);
  }

  public SmallBoard(int nextFigIndex, int nextFigColor, int score) {
    this.nextFigIndex = nextFigIndex;
    this.nextFigColor = nextFigColor;
    this.score = score;
    this.bf = new BoardFigure(nextFigIndex, 0, nextFigColor, nextFigIndex == 5 ? 2 : 1, nextFigIndex == 5 ? 2 : 1);
  }

  public String renderLine(int y) {
    if (y > NEXT_P_BOARD_H + 2) return "";
    if (y == 0) return " " + String.format("%06d", score);
    if (y == 1) return (new StringBuilder()).append(" ").append(CEIL.repeat(NEXT_P_BOARD_W)).append(" ").toString();
    if (y == NEXT_P_BOARD_H + 2) return (new StringBuilder()).append(" ").append(FLOOR.repeat(NEXT_P_BOARD_W)).append(" ").toString();

    StringBuilder line = new StringBuilder();
    for (int x = 0; x < NEXT_P_BOARD_W; x++) {
      final int xx = x;
      final int yy = y;
      boolean isPiece = Arrays.stream(bf.getCoords()).anyMatch(pair -> {
          int xc = pair[0];
          int yc = pair[1];
          return xc == xx && yc == yy - 2;
        });
      if (isPiece) {
        line.append(COLORS[nextFigColor]).append(" ").append(RESET);
      } else {
        line.append(" ");
      }
    }

    return LEFT + line.toString() + RIGHT;
  }

  @Override
  public String toString() {
    return String.valueOf(nextFigIndex) + " " + String.valueOf(nextFigColor) + " " + String.valueOf(score);
  }
}

class Board {
  private static final int BOARD_H = 20;
  private static final int BOARD_W = 10;
  private static String RESET = "\u001B[m";
  private static String INVERSE = "\u001B[7m";
  private static String CEIL = "\u2582";
  private static String FLOOR = INVERSE + "\u2586" + RESET;
  private static String LEFT = INVERSE + "\u258a" + RESET;
  private static String RIGHT = "\u258e";
  private static String SPACER = ".";

  private static final int[] SCORES = { 10, 30, 60, 100 };
  private static final String[] COLORS = {
    "", // for empty
    "\033[41m", // BackgroundRed
    "\033[42m", // BackgroundGreen
    "\033[43m", // BackgroundYellow
    "\033[44m", // BackgroundBlue
    "\033[45m", // BackgroundMagenta
    "\033[46m", // BackgroundCyan
    "\033[47m"  // BackgroundWhite
  };

  private int[][] board;
  private SmallBoard sb;
  private BoardFigure bf;

  public Board() {
    board = new int[BOARD_H][BOARD_W];
    bf = new BoardFigure();
    sb = new SmallBoard();
  }

  public Board(String strBoard, BoardFigure pbf, SmallBoard psb) {
    board = new int[BOARD_H][BOARD_W];
    for (int i = 0; i < BOARD_H; i++) {
      for (int j = 0; j < BOARD_W; j++) {
        board[i][j] = Integer.parseInt(String.valueOf(strBoard.charAt(j + i * BOARD_W)));
      }
    }
    bf = pbf;
    sb = psb;
  }

  @Override
  public String toString() {
    StringBuilder strBoard = new StringBuilder();
    for (int i = 0; i < BOARD_H; i++) {
      for (int j = 0; j < BOARD_W; j++) {
        strBoard.append(board[i][j]);
      }
    }
    return String.format("%s %s %s", strBoard, bf, sb);
  }

  private static boolean isLegalCoords(int[][] coords)  {
    return Arrays.stream(coords).allMatch(pair -> {
        int x = pair[0];
        int y = pair[1];
        return y < BOARD_H && x >= 0 && x < BOARD_W;
      });
  }

  private boolean isBoardCellsFree(int[][] coords) {
    return Arrays.stream(coords).allMatch(pair -> {
        int x = pair[0];
        int y = pair[1];
        // y < 0 - don't care about segments above top of the screen 
        return y < 0 || board[y][x] == 0;
      });
  }

  private boolean canPlace() {
    int[][] coords = bf.getCoords();
    return isLegalCoords(coords) && isBoardCellsFree(coords);
  }

  private boolean tryMoveDown() {
    bf.moveDown();
    boolean can = canPlace();
    if (!can) bf.moveUp();
    return can;
  }

  private boolean endGame() {
    return !canPlace();
  }

  private void addFigure() {
    Arrays.stream(bf.getCoords()).forEach(pair -> {
        int x = pair[0];
        int y = pair[1];
        // y < 0 - don't care about segments above top of the screen
        if (y < 0) return;
        board[y][x] = bf.color;
      });
  }

  private void clearFigure() {
    Arrays.stream(bf.getCoords()).forEach(pair -> {
        int x = pair[0];
        int y = pair[1];
        // y < 0 - don't care about segments above top of the screen
        if (y < 0) return;
        board[y][x] = 0;
      });
  }

  private int removeFullLines() {
    int countFullLines = 0;
    for (int y = 0; y < BOARD_H; y++) {
      boolean full = Arrays.stream(board[y]).allMatch(c -> c != 0);
      if (full) {
        for (int yy = y; yy > 0; yy--) {
          board[yy] = board[yy - 1];
        }
        board[0] = new int[BOARD_W];
        countFullLines++;
      }
    }

    return countFullLines;
  }

  private String renderLine(int y) {
    StringBuffer res = new StringBuffer();
    for(int x = 0; x < BOARD_W; x++) {
      if (board[y][x] != 0) {
        res.append(COLORS[board[y][x]]).append(" ").append(RESET);
      } else {
        res.append((x % 2 == 0) ? " " : SPACER);
      }
    }
    return res.toString();
  }

  public String render() {
    StringBuffer res = new StringBuffer();
    addFigure();

    res.append(" ").append(CEIL.repeat(BOARD_W)).append(" \n");

    for(int y = 0; y < BOARD_H; y++) {
      res.append(LEFT).append(renderLine(y)).append(RIGHT).append(sb.renderLine(y)).append("\n");
    }
    res.append(" ").append(FLOOR.repeat(BOARD_W)).append(" ");

    clearFigure();
    return res.toString();
  }

  public void moveDown() {
    boolean can = tryMoveDown();
    if (!can) {
      addFigure();
      int fl = removeFullLines();
      if (fl > 0) sb.score += SCORES[fl - 1];
      bf = new BoardFigure(sb.nextFigIndex, sb.nextFigColor);
      sb = new SmallBoard(sb.score);
      if (endGame()) {
        System.out.println("Game over!");
        System.exit(0);
      }
    }
  }

  public void moveLeft() {
    bf.moveLeft();
    if (!canPlace()) bf.moveRight();
  }

  public void moveRight() {
    bf.moveRight();
    if (!canPlace()) bf.moveLeft();
  }

  public void rotateClockwise() {
    bf.rotateClockwise();
    if (!canPlace()) bf.rotateCounterClockwise();
  }

  public void rotateCounterClockwise() {
    bf.rotateCounterClockwise();
    if (!canPlace()) bf.rotateClockwise();
  }

  public void drop() {
    while (tryMoveDown()) {}
    addFigure();
    int fl = removeFullLines();
    if (fl > 0) sb.score += SCORES[fl - 1];
    bf = new BoardFigure(sb.nextFigIndex, sb.nextFigColor);
    sb = new SmallBoard(sb.score);
    if (endGame()) {
        System.out.println("Game over!");
        System.exit(0);
    }
  }

}

class Game {
  
  enum MOVES {
    DOWN,
    LEFT,
    RIGHT,
    ROTATE_CLOCKWISE,
    ROTATE_COUNTER_CLOCKWISE,
    DROP
  };

  private MOVES move;
  private Board board;

  public Game(String[] args) {
    move = MOVES.values()[Integer.parseInt(args[0])];
    BoardFigure bf = new BoardFigure(Integer.parseInt(args[2]), Integer.parseInt(args[3]), Integer.parseInt(args[4]), Integer.parseInt(args[5]), Integer.parseInt(args[6]));
    SmallBoard sb = new SmallBoard(Integer.parseInt(args[7]), Integer.parseInt(args[8]), Integer.parseInt(args[9]));
    board = new Board(args[1], bf, sb);
  }

  public Game() {
    move = MOVES.DOWN;
    board = new Board();
  }

  public void update() {
    switch(move) {
    case DOWN:
      board.moveDown();
      break;
    case LEFT:
      board.moveLeft();
      break;
    case RIGHT:
      board.moveRight();
      break;
    case ROTATE_CLOCKWISE:
      board.rotateClockwise();
      break;
    case ROTATE_COUNTER_CLOCKWISE:
      board.rotateCounterClockwise();
      break;
    case DROP:
      board.drop();
      break;
    }
  }

  public String getNextState() {
    return String.format("%d %s", move.ordinal(), board);
  }  

  public String render() {
    return board.render();
  }
}

