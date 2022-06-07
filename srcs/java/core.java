//import java.util.Arrays;
// import java.util.Random;

class core {
    public static void main(String args[]) {
	// System.out.println(Arrays.toString(args));
	// System.out.println(Arrays.toString(args));
	if (args.length == 10) {
	    Game game = new Game(args);
	    //game.update();
// 	    System.out.println(game.getNextState());
	    //game.render();
	} else if (args.length == 1 && args[0].equals("INIT_STATE")) {
	    Game game = new Game();
	    //game.update();
	    System.out.println(game.getNextState());
	    System.out.println(game.getNextState());
	    //game.render();
	}
    }
}

class Rotation {
    private int[][] squares;
    private int h;
    private int w;
    private int ofx;
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
	    new Rotation(new int[][] {{0, 0}, {1, 0}, {2, 0}, {3, 0}}, 1, 4, 0, 2),
	    new Rotation(new int[][] {{0, 0}, {0, 1}, {0, 2}, {0, 3}}, 4, 1, 2, 0)
	    }),
	new Figure(new Rotation[] { // L
	    new Rotation(new int[][] {{0, 0}, {1, 0}, {2, 0}, {0, 1}}, 2, 3, 0, 1),
	    new Rotation(new int[][] {{0, 0}, {1, 0}, {1, 1}, {1, 2}}, 3, 2, 0, 0),
	    new Rotation(new int[][] {{0, 1}, {1, 1}, {2, 1}, {2, 0}}, 2, 3, 0, 0),
	    new Rotation(new int[][] {{0, 0}, {0, 1}, {0, 2}, {1, 2}}, 3, 2, 1, 0)
	    }),
	new Figure(new Rotation[] { // J
	    new Rotation(new int[][] {{0, 0}, {1, 0}, {2, 0}, {2, 1}}, 2, 3, 0, 1),
	    new Rotation(new int[][] {{1, 0}, {1, 1}, {1, 2}, {0, 2}}, 3, 2, 0, 0),
	    new Rotation(new int[][] {{0, 0}, {0, 1}, {1, 1}, {2, 1}}, 2, 3, 0, 0),
	    new Rotation(new int[][] {{0, 0}, {1, 0}, {0, 1}, {0, 2}}, 3, 2, 1, 0)
	    }),
	new Figure(new Rotation[] { // S
	    new Rotation(new int[][] {{1, 0}, {2, 0}, {0, 1}, {1, 1}}, 2, 3, 0, 1),
	    new Rotation(new int[][] {{0, 0}, {0, 1}, {1, 1}, {1, 2}}, 3, 2, 1, 0)
	    }),
	new Figure(new Rotation[] { // Z
	    new Rotation(new int[][] {{0, 0}, {1, 0}, {1, 1}, {2, 1}}, 2, 3, 0, 1),
	    new Rotation(new int[][] {{0, 1}, {1, 0}, {1, 1}, {0, 2}}, 3, 2, 1, 0)
	    }),
	new Figure(new Rotation[] { // O
	    new Rotation(new int[][] {{0, 0}, {1, 0}, {2, 0}, {3, 0}}, 1, 4, 0, 2)
	    }),
	new Figure(new Rotation[] { // T
	    new Rotation(new int[][] {{0, 0}, {1, 0}, {2, 0}, {1, 1}}, 2, 3, 0, 1),
	    new Rotation(new int[][] {{1, 0}, {0, 1}, {1, 1}, {1, 2}}, 3, 2, 0, 0),
	    new Rotation(new int[][] {{1, 0}, {0, 1}, {1, 1}, {2, 1}}, 2, 3, 0, 0),
	    new Rotation(new int[][] {{0, 0}, {0, 1}, {0, 2}, {1, 1}}, 3, 2, 1, 0)
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
	    "\033[47m"  // BackgroundWhite
    };

    private Figure figure;
    private int figIndex;
    private int rotateIndex;
    private int color;
    private int offsetX;
    private int offsetY;
    
    public static int getRandomIntInclusive(int min, int max) {
        return (int)(Math.random() * (max - min + 1)) + min;
    }
    
    @Override
    public String toString() {
        return String.valueOf(figIndex) + " " +  String.valueOf(rotateIndex) + " " + String.valueOf(color) + " " + String.valueOf(offsetX)
         + " " + String.valueOf(offsetY);  
    }
    
    public BoardFigure() {
	    this.figIndex = getRandomIntInclusive(0, FIGURES.length - 1);
        this.rotateIndex = 0;
        this.color = getRandomIntInclusive(0, COLORS.length - 1);
	    this.figure = FIGURES[figIndex];
	    this.offsetX = figIndex == 0 ? 3 : 4;
	    this.offsetY = -1 * FIGURES[figIndex].rotations[rotateIndex].ofy;
    }
        public BoardFigure(/* Figure figure, */ int figIndex, int rotateIndex, int color /* , int offsetX, int offsetY */) {
        }
}

class Game {
    private static final int BOARD_H = 20; 
    private static final int BOARD_W = 10; 
    private static final int[] SCORES = {10, 30, 60, 100};
    enum MOVES {
	    DOWN,
	    LEFT,
	    RIGHT,
	    ROTATE_CLOCKWISE,
	    ROTATE_COUNTER_CLOCKWISE,
    };


//     private static final Rotation r = new Rotation(2);
// private static final Figure f = new Figure(new Rotation[] {r});


    private MOVES move;
    private int[][] board;
    private BoardFigure bf;
    private int nextFigIndex;
    private int nextFigColor;
    private int score;
//  printf("%u %s %u %u %u %d %d %u %u %u\n", state->move, strBoard, state->figIndex, state->rotateIndex, state->color,
//          state->offsetX, state->offsetY, state->nextFigIndex, state->nextFigColor, state->score);

    public String getNextState() {
        StringBuilder strBoard = new StringBuilder();
        for(int i = 0; i < BOARD_H; i++) {
            for(int j = 0; j < BOARD_W; j++) {
                strBoard.append(board[i][j]);
            }
        }
        return String.format("%d %s %s %d %d %d", move.ordinal(), strBoard, bf, nextFigIndex, nextFigColor, score);
    }

    public Game(String[] args) {
	
    }

    


    public Game() {
	this.move = MOVES.DOWN;
	this.board = new int [BOARD_H][BOARD_W];
	this.bf = new BoardFigure();
	this.score = 0;
    }
}
