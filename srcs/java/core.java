import java.util.Arrays;
import java.util.ArrayList;
import java.util.stream.IntStream;

class core {
    public static void main(String args[]) {
        if (args.length == 10) {
            Game game = new Game(args);
            game.update();
	    //            System.out.println(game.getNextState());
            System.out.println(game.getNextState());
            System.out.println(game.render());
        } else if (args.length == 1 && args[0].equals("INIT_STATE")) {
            Game game = new Game();
	    //System.out.println(game.getNextState());
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
    private int h;
    public int w;
    public int ofx;
    public int ofy;

    public Rotation(int[][] squares, int h, int w, int ofx, int ofy) {
        this.squares = squares;
        this.h = h;
        this.w = w;
        this.ofx = ofx;
        this.ofy = ofy;
    }
    public Rotation(int ofy) {
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
		new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 3, 0 } }, 1, 4, 0, 2),
		new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 0, 3 } }, 4, 1, 2, 0)
	    }),
        new Figure(new Rotation[] { // L
		new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 0, 1 } }, 2, 3, 0, 1),
		new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 1, 1 }, { 1, 2 } }, 3, 2, 0, 0),
		new Rotation(new int[][] { { 0, 1 }, { 1, 1 }, { 2, 1 }, { 2, 0 } }, 2, 3, 0, 0),
		new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 1, 2 } }, 3, 2, 1, 0)
	    }),
        new Figure(new Rotation[] { // J
		new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 2, 1 } }, 2, 3, 0, 1),
		new Rotation(new int[][] { { 1, 0 }, { 1, 1 }, { 1, 2 }, { 0, 2 } }, 3, 2, 0, 0),
		new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 2, 1 } }, 2, 3, 0, 0),
		new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 0, 1 }, { 0, 2 } }, 3, 2, 1, 0)
	    }),
        new Figure(new Rotation[] { // S
		new Rotation(new int[][] { { 1, 0 }, { 2, 0 }, { 0, 1 }, { 1, 1 } }, 2, 3, 0, 1),
		new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 1, 2 } }, 3, 2, 1, 0)
	    }),
        new Figure(new Rotation[] { // Z
		new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 1, 1 }, { 2, 1 } }, 2, 3, 0, 1),
		new Rotation(new int[][] { { 0, 1 }, { 1, 0 }, { 1, 1 }, { 0, 2 } }, 3, 2, 1, 0)
	    }), 
        new Figure(new Rotation[] { // O
		new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 1, 0 } }, 2, 2, 0, 0)
	    }),
        new Figure(new Rotation[] { // T
		new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 1, 1 } }, 2, 3, 0, 1),
		new Rotation(new int[][] { { 1, 0 }, { 0, 1 }, { 1, 1 }, { 1, 2 } }, 3, 2, 0, 0),
		new Rotation(new int[][] { { 1, 0 }, { 0, 1 }, { 1, 1 }, { 2, 1 } }, 2, 3, 0, 0),
		new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 1, 1 } }, 3, 2, 1, 0)
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


    public Figure figure;
    private int figIndex;
    public int rotateIndex;
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
        figIndex = Random.getRandomIntInclusive(0, FIGURES.length - 1);
        rotateIndex = 0;
        color = Random.getRandomIntInclusive(1, COLORS.length - 1);
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
        ArrayList<int[]> res = new ArrayList();
        for(int[] pair:figure.rotations[rotateIndex].squares) {
            int[] r = {
		pair[0] + offsetX + figure.rotations[rotateIndex].ofx,
		pair[1] + offsetY + figure.rotations[rotateIndex].ofy
            };
            if (r[1] >= 0) {
                res.add(r);
            }
        }
        
        return res.toArray(new int [res.size()][]);
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
        rotateIndex = rotClockwiseIndex();
    }
    
    public int rotClockwiseIndex() {
        return (rotateIndex == FIGURES[figIndex].rotations.length - 1) ? 0 : rotateIndex + 1;
    }

    public void rotateCounterClockwise() {
	rotateIndex = rotCounterClockwiseIndex();
    }
    
    public int rotCounterClockwiseIndex() {
	return (rotateIndex == 0) ? FIGURES[figIndex].rotations.length - 1 : rotateIndex - 1;
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
    private int nextFigIndex;
    private int nextFigColor;
    private int score;
    private BoardFigure bf;

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
    
    private int[][] board;
    public SmallBoard sb;
    
    public Board(SmallBoard sb) {
        board = new int[BOARD_H][BOARD_W];
	this.sb = sb;
    }
    
    public Board(String strBoard, SmallBoard sb) {
        board = new int[BOARD_H][BOARD_W];
        for (int i = 0; i < BOARD_H; i++) {
            for (int j = 0; j < BOARD_W; j++) {
                board[i][j] = Integer.parseInt(String.valueOf(strBoard.charAt(j + i * BOARD_W)));
            }
        }
	this.sb = sb;
    }

    @Override
    public String toString() {
        StringBuilder strBoard = new StringBuilder();
        for (int i = 0; i < BOARD_H; i++) {
            for (int j = 0; j < BOARD_W; j++) {
                strBoard.append(board[i][j]);
            }
        }
        return strBoard.toString();
    }
    
    public boolean needNewFigure(BoardFigure f) {
        return Arrays.stream(f.getCoords()).anyMatch(pair -> {
		int x = pair[0];
		int y = pair[1];
		return y + 1 == BOARD_H || board[y + 1][x] != 0;
	    });
    }
    
    public void addFigure(BoardFigure f) {
        Arrays.stream(f.getCoords()).forEach(pair -> {
		int x = pair[0];
		int y = pair[1];
		board[y][x] = f.color;
	    });    
    }
    
    private void clearFigure(BoardFigure f) {
	Arrays.stream(f.getCoords()).forEach(pair -> {
		int x = pair[0];
		int y = pair[1];
		board[y][x] = 0;
	    });
    }

    public int removeFullLines() {
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
    
    public boolean endGame(BoardFigure f) {
        boolean overlap = Arrays.stream(f.getCoords()).anyMatch(pair -> {
		int x = pair[0];
		int y = pair[1];
		return board[y][x] != 0;
	    });
	if (overlap) {

	}
	return overlap;
    }

    public boolean canMoveLeft(BoardFigure f) {
        if (f.offsetX + f.figure.rotations[f.rotateIndex].ofx <= 0)
            return false;
     
        return Arrays.stream(f.getCoords()).allMatch(pair -> {
		int x = pair[0];
		int y = pair[1];
		return board[y][x - 1] == 0;
	    });
    }

    public boolean canMoveRight(BoardFigure f) {
        if (f.offsetX + f.figure.rotations[f.rotateIndex].w + f.figure.rotations[f.rotateIndex].ofx >= BOARD_W)
            return false;
        
        return Arrays.stream(f.getCoords()).allMatch(pair -> {
		int x = pair[0];
		int y = pair[1];
		return board[y][x + 1] == 0;
	    });
    }
    
    public boolean canRotateClockwise(BoardFigure f) {
        f.rotateClockwise();
        boolean canR = Arrays.stream(f.getCoords()).allMatch(pair -> {
		int x = pair[0];
		int y = pair[1];
		return x >= 0 && x < BOARD_W && y < BOARD_H && board[y][x] == 0;
	    });
        f.rotateCounterClockwise();
        return canR;
    }

    public boolean canRotateCounterClockwise(BoardFigure f) {
        f.rotateCounterClockwise();
        boolean canR = Arrays.stream(f.getCoords()).allMatch(pair -> {
		int x = pair[0];
		int y = pair[1];
		return x >= 0 && x < BOARD_W && y < BOARD_H && board[y][x] == 0;
	    });
	f.rotateClockwise();
        return canR;
    }

    public void drop(BoardFigure f) {
	while (!needNewFigure(f)) {
	    f.moveDown();
	}
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
    
    public String render(BoardFigure f) {
        StringBuffer res = new StringBuffer();
        addFigure(f);
        
        res.append(" ").append(CEIL.repeat(BOARD_W)).append(" \n");

        for(int y = 0; y < BOARD_H; y++) {
            res.append(LEFT).append(renderLine(y)).append(RIGHT).append(sb.renderLine(y)).append("\n");
        }
        res.append(" ").append(FLOOR.repeat(BOARD_W)).append(" ");
        
        clearFigure(f);
        return res.toString();
    }
}

class Game {
    private static final int[] SCORES = { 10, 30, 60, 100 };
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
    private SmallBoard sb;
    private BoardFigure bf;
    private int nextFigIndex;
    private int nextFigColor;
    private int score;

    public String getNextState() {
        return String.format("%d %s %s %d %d %d", move.ordinal(), board, bf, nextFigIndex, nextFigColor, score);
    }

    public Game(String[] args) {
        move = MOVES.values()[Integer.parseInt(args[0])];
        bf = new BoardFigure(Integer.parseInt(args[2]), Integer.parseInt(args[3]), Integer.parseInt(args[4]), Integer.parseInt(args[5]), Integer.parseInt(args[6]));
        nextFigIndex = Integer.parseInt(args[7]);
        nextFigColor = Integer.parseInt(args[8]);
        score = Integer.parseInt(args[9]);
	sb = new SmallBoard(nextFigIndex, nextFigColor, score);
        board = new Board(args[1], sb);
    }

    public Game() {
        move = MOVES.DOWN;
        bf = new BoardFigure();
        nextFigIndex = BoardFigure.getNextFigIndex();
        nextFigColor = BoardFigure.getNextFigColor();
        score = 0;
	sb = new SmallBoard(nextFigIndex, nextFigColor, score);
        board = new Board(sb);
    }
    
    public void update() {
	//      System.out.println(move);
	switch(move) {
	case DOWN:
	    if (board.needNewFigure(bf)) {
		board.addFigure(bf);
		int fl = board.removeFullLines();
		if (fl > 0) score += SCORES[fl - 1];
		bf = new BoardFigure(nextFigIndex, nextFigColor);
		nextFigIndex = BoardFigure.getNextFigIndex();
		nextFigColor = BoardFigure.getNextFigColor();
		board.sb = new SmallBoard(nextFigIndex, nextFigColor, score);
		if (board.endGame(bf)) {
		    System.out.println("Game over!");
		    System.exit(0);  
		}
		break;
	    }
	    bf.moveDown();
	    break;
	case LEFT:
	    if (board.canMoveLeft(bf)) {
		bf.moveLeft();
	    }
	    break;
	case RIGHT:
	    if (board.canMoveRight(bf)) {
		bf.moveRight();
	    }
	    break;
	case ROTATE_CLOCKWISE:
	    if (board.canRotateClockwise(bf)) {
		bf.rotateClockwise();
	    }
	    break;
	case ROTATE_COUNTER_CLOCKWISE:
	    if (board.canRotateCounterClockwise(bf)) {
		bf.rotateCounterClockwise();
	    }
	    break;
	case DROP:
	    board.drop(bf);
	    board.addFigure(bf);
	    int fl = board.removeFullLines();
	    if (fl > 0) score += SCORES[fl - 1];
	    bf = new BoardFigure(nextFigIndex, nextFigColor);
	    nextFigIndex = BoardFigure.getNextFigIndex();
	    nextFigColor = BoardFigure.getNextFigColor();
	    board.sb = new SmallBoard(nextFigIndex, nextFigColor, score);
	    if (board.endGame(bf)) {
		System.out.println("Game over!");
		System.exit(0);
	    }
	    break;
        }
    }
    
    public String render() {
        return board.render(bf);
    }
}
