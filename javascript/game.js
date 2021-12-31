const Colors = [
  '', // for empty
  '\u001b[31m', // Red
  '\u001b[32m', // Green
  '\u001b[33m', // Yellow
  '\u001b[34m', // Blue
  '\u001b[35m', // Magenta
  '\u001b[36m', // Cyan
  '\u001b[37m', // White
]
const BackColors = [
  '', // for empty
  '\u001b[41m', // BackgroundRed
  '\u001b[42m', // BackgroundGreen
  '\u001b[43m', // BackgroundYellow
  '\u001b[44m', // BackgroundBlue
  '\u001b[45m', // BackgroundMagenta
  '\u001b[46m', // BackgroundCyan
  '\u001b[47m', // BackgroundWhite
]
const Reset = '\u001b[m'
const Inverse = '\u001B[7m'
const boardW = 10
const boardH = 20

const getRandomIntInclusive = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min
}
const moves = {
  auto: 0,
  left: 1,
  right: 2,
  down: 3,
  rotateClockwise: 4,
  rotateCounterClockwise: 5,
}

const states = {
  start: 0,
  newPiece: 1,
  move: 2
}
const figures = [
  [
    { squares: [[0, 0], [1, 0], [2, 0], [3, 0]], h: 1, w: 4, ofx: 0, ofy: 2 },
    { squares: [[0, 0], [0, 1], [0, 2], [0, 3]], h: 4, w: 1, ofx: 2, ofy: 0 },
  ], // I
  [
    { squares: [[0, 0], [1, 0], [2, 0], [0, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[0, 0], [1, 0], [1, 1], [1, 2]], h: 3, w: 2, ofx: 0, ofy: 0 },
    { squares: [[0, 1], [1, 1], [2, 1], [2, 0]], h: 2, w: 3, ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [0, 2], [1, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ], // L
  [
    { squares: [[0, 0], [1, 0], [2, 0], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[1, 0], [1, 1], [1, 2], [0, 2]], h: 3, w: 2, ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 0 },
    { squares: [[0, 0], [1, 0], [0, 1], [0, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ], // Г
  [
    { squares: [[1, 0], [2, 0], [0, 1], [1, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[0, 0], [0, 1], [1, 1], [1, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ], // S
  [
    { squares: [[0, 0], [1, 0], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[0, 1], [1, 0], [1, 1], [0, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ], // Z
  [
    { squares: [[0, 0], [0, 1], [1, 1], [1, 0]], h: 2, w: 2, ofx: 0, ofy: 0 },
  ], // O
  [
    { squares: [[0, 0], [1, 0], [2, 0], [1, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[1, 0], [0, 1], [1, 1], [1, 2]], h: 3, w: 2, ofx: 0, ofy: 0 },
    { squares: [[1, 0], [0, 1], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [0, 2], [1, 1]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ]  // T
]
// const figOffset = [-3, -2, -2, -2, -2, -1, -2]

const state = {
  board: [], active: [], rotation: 0, offset: [], figCoords: [], move: 0, color: 0, figIndex: 0, state: 1
}

const init = (state) => {
  for (let y = 0; y < boardH; y++) {
    const row = Array(boardW).fill(0)
    state.board.push(row)
  }

  state.move = 0
}
const board = (x, y) => state.board[y][x]
const boardIsFree = (x, y) => {
  if (x < 0 || x > boardW || y < 0 || y > boardH)
    return true
  return board(x, y) === 0
}

const newPiece = (state) => {
  state.figIndex = getRandomIntInclusive(0, figures.length - 1)
  state.rotation = 0 //getRandomIntInclusive(0, state.active.length - 1)
  state.active = figures[state.figIndex][state.rotation]
  state.offset = [state.figIndex === 0 ? 3 : 4, -1 * state.active.ofy]
  state.color = getRandomIntInclusive(1, BackColors.length - 1)
}
const canMoveRight = () => {
  const piece = state.active
  return (state.offset[0] + piece.w + piece.ofx < boardW) && state.figCoords.every(([x, y]) => boardIsFree(x + 1, y))
}

const canMoveLeft = () => {
  const piece = state.active
  return (state.offset[0] + piece.ofx > 0) && state.figCoords.every(([x, y]) => boardIsFree(x - 1, y))
}

const canRotate = (newRot) => {
  const piece = figures[state.figIndex][newRot]
  const pieceFigCoords = getFigCoords(piece, state.offset)
  return pieceFigCoords.every(([x, y]) => x >= 0 && x < boardW && boardIsFree(x, y))
}
const handleMove = state => {
  switch (state.move) {
    case moves.auto:
    case moves.down:
      state.offset[1] += 1
      break
    case moves.right:
      // if (state.offset[1] === figOffset[state.figIndex]) {
      //   state.offset[1] += 1
      //   break
      // }
      if (canMoveRight())
        state.offset[0] += 1
      break
    case moves.left:
      // if (state.offset[1] === figOffset[state.figIndex]) {
      //   state.offset[1] += 1
      //   break
      // }
      if (canMoveLeft())
        state.offset[0] -= 1
      break
    case moves.rotateClockwise: {
      const newRot = state.rotation === figures[state.figIndex].length - 1 ? 0 : state.rotation + 1
      if (canRotate(newRot)) {
        state.rotation = newRot
      }
      state.active = figures[state.figIndex][state.rotation]
      break
    }
    case moves.rotateCounterClockwise: {
      const newRot = state.rotation === 0 ? figures[state.figIndex].length - 1 : state.rotation - 1
      if (canRotate(newRot)) {
        state.rotation = newRot
      }
      state.active = figures[state.figIndex][state.rotation]
      break
    }
  }
}
const update = (state) => {
  switch (state.state) {
    case states.start:
      state.state = states.newPiece
      break
    case states.newPiece:
      newPiece(state)
      getFigCoords(state.active, state.offset).forEach(([x, y]) => {
        state.board[y][x] = state.color
      })
      state.state = states.move
      break
    case states.move:
      getFigCoords(state.active, state.offset).forEach(([x, y]) => {
        state.board[y][x] = 0
      })
      switch (state.move) {
        case moves.auto:
        case moves.down:
          const newFigCoords = getFigCoords(state.active, [state.offset[0], state.offset[1] + 1])
          if (newFigCoords.some(([x, y]) => y === boardH || !boardIsFree(x, y))) {
            state.figCoords.forEach(([x, y]) => {
              state.board[y][x] = state.color
            })
            state.state = states.newPiece
            break
          } else {
            
          }
          state.offset[1] += 1
          break
        case moves.right:
          // if (state.offset[1] === figOffset[state.figIndex]) {
          //   state.offset[1] += 1
          //   break
          // }
          if (canMoveRight())
            state.offset[0] += 1
          break
        case moves.left:
          // if (state.offset[1] === figOffset[state.figIndex]) {
          //   state.offset[1] += 1
          //   break
          // }
          if (canMoveLeft())
            state.offset[0] -= 1
          break
        case moves.rotateClockwise: {
          const newRot = state.rotation === figures[state.figIndex].length - 1 ? 0 : state.rotation + 1
          if (canRotate(newRot)) {
            state.rotation = newRot
          }
          state.active = figures[state.figIndex][state.rotation]
          break
        }
        case moves.rotateCounterClockwise: {
          const newRot = state.rotation === 0 ? figures[state.figIndex].length - 1 : state.rotation - 1
          if (canRotate(newRot)) {
            state.rotation = newRot
          }
          state.active = figures[state.figIndex][state.rotation]
          break
        }
      }
      const newFigCoords = getFigCoords(state.active, state.offset)
      if (newFigCoords.some(([x, y]) => y === boardH || !boardIsFree(x, y))) {
        state.figCoords.forEach(([x, y]) => {
          state.board[y][x] = state.color
        })
        state.state = states.newPiece
        break
      } else {
        newFigCoords.forEach(([x, y]) => {
          state.board[y][x] = state.color
        })
      }
  }

  // if (newFigCoords.some(([x, y]) => y === boardH || !boardIsFree(x, y))) {
  //   state.figCoords.forEach(([x, y]) => {
  //     state.board[y][x] = state.color
  //   })
  //
  //   newPiece(state)
  // } else {
  //   state.figCoords = newFigCoords
  // }
  // const boardWithoutFillLines = state.board.filter(line => line.some(c => c === 0))
  //
  // if (boardWithoutFillLines.length < boardH) {
  //   const newLines = []
  //   for (let i = 0; i < boardH - boardWithoutFillLines.length; i++) {
  //     newLines.push(Array(boardW).fill(0))
  //   }
  //   boardWithoutFillLines.unshift(...newLines)
  //   state.board = boardWithoutFillLines
  //
  // }

  // if (state.figCoords.some(([x, y]) => y + 1 === boardH || !boardIsFree(x, y + 1))) {
  //   state.figCoords.forEach(([x, y]) => {
  //     state.board[y][x] = state.color
  //   })
  //
  //   newPiece(state)
  // }
}
const getFigCoords = (fig, offset) => fig.squares.map(([x, y]) => [x + offset[0] + fig.ofx, y + offset[1] + fig.ofy])

const render = (state) => {
  let res = ''
  for (let x = 0; x < boardW + 2; x++) {
    res += '.'
  }
  res += '\n'

  for (let y = 0; y < boardH; y++) {
    res += '.'
    for (let x = 0; x < boardW; x++) {
      if (board(x, y) !== 0) {
        res += BackColors[board(x, y)] + ' ' + Reset
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

init(state)
const step = state => {
  update(state)
  console.log(state.state)
  console.log(render(state))
}
const int = setInterval(() => {
  if (state !== 'game_over') {
    state.move = moves.auto
    step(state)
  } else {
    clearInterval(int)
  }
}, 400)
const stdin = process.stdin
stdin.setRawMode(true)
// console.log(stdin.isTTY)
stdin.setEncoding('utf8')

stdin.on('data', chunk => {
  // var option = chunk.substr(0, chunk.length - 1)
  switch (chunk) {
    case '\u001b[A':
      // console.log('Up')
      // state.offset

      break
    case '\u001b[B':
      state.move = moves.down
      step(state)
      // console.log('Down')
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
