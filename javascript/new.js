const backColors = ['', // for empty
  '\u001b[41m', // BackgroundRed
  '\u001b[42m', // BackgroundGreen
  '\u001b[43m', // BackgroundYellow
  '\u001b[44m', // BackgroundBlue
  '\u001b[45m', // BackgroundMagenta
  '\u001b[46m', // BackgroundCyan
  '\u001b[47m', // BackgroundWhite
]
const Reset = '\u001b[m'
const boardW = 10
const boardH = 20
const moves = {
  auto: 0, left: 1, right: 2, down: 3, rotateClockwise: 4, rotateCounterClockwise: 5,
}
const states = {
  start: 0, newPiece: 1, move: 2, beforeNewPiece: 3, end: 4
}
const figures = [[{
  squares: [[0, 0], [1, 0], [2, 0], [3, 0]], h: 1, w: 4, ofx: 0, ofy: 2
}, { squares: [[0, 0], [0, 1], [0, 2], [0, 3]], h: 4, w: 1, ofx: 2, ofy: 0 },], // I
  [{
    squares: [[0, 0], [1, 0], [2, 0], [0, 1]], h: 2, w: 3, ofx: 0, ofy: 1
  }, {
    squares: [[0, 0], [1, 0], [1, 1], [1, 2]], h: 3, w: 2, ofx: 0, ofy: 0
  }, {
    squares: [[0, 1], [1, 1], [2, 1], [2, 0]], h: 2, w: 3, ofx: 0, ofy: 0
  }, { squares: [[0, 0], [0, 1], [0, 2], [1, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },], // L
  [{
    squares: [[0, 0], [1, 0], [2, 0], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 1
  }, {
    squares: [[1, 0], [1, 1], [1, 2], [0, 2]], h: 3, w: 2, ofx: 0, ofy: 0
  }, {
    squares: [[0, 0], [0, 1], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 0
  }, { squares: [[0, 0], [1, 0], [0, 1], [0, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },], // Г
  [{
    squares: [[1, 0], [2, 0], [0, 1], [1, 1]], h: 2, w: 3, ofx: 0, ofy: 1
  }, { squares: [[0, 0], [0, 1], [1, 1], [1, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },], // S
  [{
    squares: [[0, 0], [1, 0], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 1
  }, { squares: [[0, 1], [1, 0], [1, 1], [0, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },], // Z
  [{ squares: [[0, 0], [0, 1], [1, 1], [1, 0]], h: 2, w: 2, ofx: 0, ofy: 0 },], // O
  [{
    squares: [[0, 0], [1, 0], [2, 0], [1, 1]], h: 2, w: 3, ofx: 0, ofy: 1
  }, {
    squares: [[1, 0], [0, 1], [1, 1], [1, 2]], h: 3, w: 2, ofx: 0, ofy: 0
  }, {
    squares: [[1, 0], [0, 1], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 0
  }, { squares: [[0, 0], [0, 1], [0, 2], [1, 1]], h: 3, w: 2, ofx: 1, ofy: 0 },]  // T
]

const getRandomIntInclusive = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min
}

const update = ({ board, figIndex, rotateIndex, color, offsetX, offsetY, move, state }) => {
  // console.log({ figIndex, rotateIndex, color, offsetX, offsetY, move, state })
  // console.log(state)
  switch (state) {
    case states.newPiece: {
      // create new piece
      figIndex = getRandomIntInclusive(0, figures.length - 1)
      rotateIndex = 0 //getRandomIntInclusive(0, figures[figIndex].length - 1)
      offsetX = figIndex === 0 ? 3 : 4
      offsetY = -1 * figures[figIndex][rotateIndex].ofy
      color = getRandomIntInclusive(1, backColors.length - 1)
      state = states.move

      const coords = getFigCoords({
        figIndex, rotateIndex, offsetX, offsetY,
      })

      // check end of game
      if (coords.some((([x, y]) => board[y][x] !== 0))) {
        state = states.end
        break
      }

      // add new piece to board
      for (const [x, y] of coords) {
        board[y][x] = color
      }
      break
    }
    case states.move: {
      // remove old piece
      const coords = getFigCoords({
        figIndex, rotateIndex, offsetX, offsetY,
      })
      for (const [x, y] of coords) {
        board[y][x] = 0
      }

      // update piece position
      switch (move) {
        case moves.auto:
        case moves.down:
          offsetY += 1
          break
        case moves.left:
          if ((offsetX + figures[figIndex][rotateIndex].ofx > 0) && coords.every(([x, y]) => board[y][x - 1] === 0)) {
            offsetX -= 1
          }
          break
        case moves.right:
          if ((offsetX + figures[figIndex][rotateIndex].w + figures[figIndex][rotateIndex].ofx < boardW) && coords.every(([x, y]) => board[y][x + 1] === 0)) {
            offsetX += 1
          }
          break
      }

      // calculate new coordinates
      const newCoords = getFigCoords({
        figIndex, rotateIndex, offsetX, offsetY,
      })

      // check is new position is overlap or on floor
      if (newCoords.some((([x, y]) => (y + 1 === boardH) || board[y + 1][x] !== 0))) {
        state = states.beforeNewPiece
        // for (const [x, y] of coords) {
        //   board[y][x] = color
        // }
      }

      // add new piece to board
      for (const [x, y] of newCoords) {
        board[y][x] = color
      }
      break
    }
    case states.beforeNewPiece: {
      // remove old piece
      const coords = getFigCoords({
        figIndex, rotateIndex, offsetX, offsetY,
      })
      for (const [x, y] of coords) {
        board[y][x] = 0
      }

      // update piece position
      switch (move) {
        case moves.auto:
        case moves.down:
          offsetY += 1
          break
        case moves.left:
          if ((offsetX + figures[figIndex][rotateIndex].ofx > 0) && coords.every(([x, y]) => board[y][x - 1] === 0)) {
            offsetX -= 1
          }
          break
        case moves.right:
          if ((offsetX + figures[figIndex][rotateIndex].w + figures[figIndex][rotateIndex].ofx < boardW) && coords.every(([x, y]) => board[y][x + 1] === 0)) {
            offsetX += 1
          }
          break
      }

      // calculate new coordinates
      const newCoords = getFigCoords({
        figIndex, rotateIndex, offsetX, offsetY,
      })

      // check is new position is overlap or on floor
      if (newCoords.some((([x, y]) => (y === boardH) || board[y][x] !== 0))) {
        for (const [x, y] of coords) {
          board[y][x] = color
        }

        // create new piece
        figIndex = getRandomIntInclusive(0, figures.length - 1)
        rotateIndex = 0 //getRandomIntInclusive(0, figures[figIndex].length - 1)
        offsetX = figIndex === 0 ? 3 : 4
        offsetY = -1 * figures[figIndex][rotateIndex].ofy
        color = getRandomIntInclusive(1, backColors.length - 1)
        state = states.move

        const newCoords = getFigCoords({
          figIndex, rotateIndex, offsetX, offsetY,
        })

        // check end of game
        if (newCoords.some((([x, y]) => board[y][x] !== 0))) {
          state = states.end
          break
        }

        // add new piece to board
        for (const [x, y] of newCoords) {
          board[y][x] = color
        }
        break
      }

      // add new piece to board
      for (const [x, y] of newCoords) {
        board[y][x] = color
      }
      break
    }
  }

  // console.log({ figIndex, rotateIndex, color, offsetX, offsetY, move, state })
  return { board, figIndex, rotateIndex, color, offsetX, offsetY, move, state }
}
const getFigCoords = ({
  figIndex, rotateIndex, offsetX, offsetY,
}) => figures[figIndex][rotateIndex].squares.map(([x, y]) => [x + offsetX + figures[figIndex][rotateIndex].ofx, y + offsetY + figures[figIndex][rotateIndex].ofy])

const render = ({ board, figIndex, rotateIndex, color, offsetX, offsetY, move, state }) => {

  // console.log(board)
  let res = ''
  for (let x = 0; x < boardW + 2; x++) {
    res += '.'
  }
  res += '\n'

  for (let y = 0; y < boardH; y++) {
    res += '.'
    for (let x = 0; x < boardW; x++) {
      if (board[y][x] !== 0) {
        res += backColors[board[y][x]] + ' ' + Reset
        // } else if (state.figCoords.some(([xc, yc]) => xc === x && yc === y)) {
        //   res += backColors[state.color] + ' ' + Reset
      } else {
        res += ' '
      }
    }
    res += '.'
    res += '\n'
  }
  for (let x = 0; x < boardW + 2; x++) {
    res += '.'
  }
  res += '\n'

  return res
}

const newBoard = []
for (let y = 0; y < boardH; y++) {
  const row = Array(boardW).fill(0)
  newBoard.push(row)
}

let state = { board: newBoard, state: states.newPiece }
const step = () => {
  state = update(state)
  console.log(render(state))
}
const int = setInterval(() => {
  if (state.state !== states.end) {
    state.move = moves.auto
    step(state)
    // state = update(state)
    // console.log(render(state))
  } else {
    clearInterval(int)
    console.log('the end')
    process.exit()
  }
}, 400)
const stdin = process.stdin
stdin.setRawMode(true)
// console.log(stdin.isTTY)
stdin.setEncoding('utf8')

stdin.on('data', chunk => {
  switch (chunk) {
    case '\u001b[A':
      // console.log('Up')
      // state.offset

      break
    case '\u001b[B':
      state.move = moves.down
      step(state)
      break
    case '\u001b[C':
      state.move = moves.right
      step(state)
      break
    case '\u001b[D':
      state.move = moves.left
      step(state)
      break
    case 'z':
      state.move = moves.rotateClockwise
      step(state)
      break
    case 'x':
      state.move = moves.rotateCounterClockwise
      step(state)
      break
    case '\u0003':
      console.log('Exit')
      process.exit()
      break
    case 'something_else':
      // Perform what something_else does
      break
  }
})