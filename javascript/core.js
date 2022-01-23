#!/usr/bin/env node

const backColors = [
  '', // for empty
  '\033[41m', // BackgroundRed
  '\033[42m', // BackgroundGreen
  '\033[43m', // BackgroundYellow
  '\033[44m', // BackgroundBlue
  '\033[45m', // BackgroundMagenta
  '\033[46m', // BackgroundCyan
  '\033[47m', // BackgroundWhite
]
const boardW = 10
const boardH = 20
const scores = [10, 30, 60, 100]
const moves = {
  tick: 0, left: 1, right: 2, down: 3, rotateClockwise: 4, rotateCounterClockwise: 5,
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

const getRandomIntInclusive = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min
}

const createBoard = (boardW, boardH) => {
  const board = []
  for (let y = 0; y < boardH; y++) {
    const row = Array(boardW).fill(0)
    board.push(row)
  }
  return board
}

/*
const paintPiece = ({ board, figIndex, rotateIndex, offsetX, offsetY, color }) => {
  const coords = getFigCoords({ figIndex, rotateIndex, offsetX, offsetY, })
  for (const [x, y] of coords) {
    board[y][x] = color
  }
}

const clearPiece = ({ board, figIndex, rotateIndex, offsetX, offsetY }) => paintPiece({
  board,
  figIndex,
  rotateIndex,
  offsetX,
  offsetY,
  color: 0
}) */

const getFigCoords = ({ figIndex, rotateIndex, offsetX, offsetY, }) =>
  figures[figIndex][rotateIndex].squares
    .map(([x, y]) => [x + offsetX + figures[figIndex][rotateIndex].ofx, y + offsetY + figures[figIndex][rotateIndex].ofy])
    .filter(([_, y]) => y >= 0)

const init = () => {
  const move = moves.tick
  const board = createBoard(boardW, boardH)
  const figIndex = getRandomIntInclusive(0, figures.length - 1)
  const rotateIndex = 0
  const color = getRandomIntInclusive(1, backColors.length - 1)
  const offsetX = figIndex === 0 ? 3 : 4
  const offsetY = -1 * figures[figIndex][rotateIndex].ofy - 1
  const nextFigIndex = getRandomIntInclusive(0, figures.length - 1)
  const nextFigColor = getRandomIntInclusive(1, backColors.length - 1)
  const score = 0

  // add new piece to board
  // paintPiece({ board, figIndex, rotateIndex, offsetX, offsetY, color })

  return { move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score }
}

const update = ({ move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score }) => {
  // console.log({ figIndex, rotateIndex, offsetX, offsetY, })
  const oldCoords = getFigCoords({ figIndex, rotateIndex, offsetX, offsetY, })

  const newPos = { rotateIndex, offsetX, offsetY }
  // update piece position
  switch (move) {
    case moves.tick:
    case moves.down:
      newPos.offsetY += 1
      break
    case moves.left:
      if ((offsetX + figures[figIndex][rotateIndex].ofx > 0) && oldCoords.every(([x, y]) => board[y][x - 1] === 0)) {
        newPos.offsetX -= 1
      }
      break
    case moves.right:
      if ((offsetX + figures[figIndex][rotateIndex].w + figures[figIndex][rotateIndex].ofx < boardW) && oldCoords.every(([x, y]) => board[y][x + 1] === 0)) {
        newPos.offsetX += 1
      }
      break
    case moves.rotateClockwise: {
      const newRotIndex = rotateIndex === figures[figIndex].length - 1 ? 0 : rotateIndex + 1
      const rotateFigCoords = getFigCoords({ figIndex, rotateIndex: newRotIndex, offsetX, offsetY })
      if (rotateFigCoords.every(([x, y]) => x >= 0 && x < boardW && board[y][x] === 0)) {
        newPos.rotateIndex = newRotIndex
      }
      break
    }
    case moves.rotateCounterClockwise: {
      const newRotIndex = rotateIndex === 0 ? figures[figIndex].length - 1 : rotateIndex - 1
      const rotateFigCoords = getFigCoords({ figIndex, rotateIndex: newRotIndex, offsetX, offsetY })
      if (rotateFigCoords.every(([x, y]) => x >= 0 && x < boardW && board[y][x] === 0)) {
        newPos.rotateIndex = newRotIndex
      }
      break
    }
  }

  // calculate new coordinates
  const newCoords = getFigCoords({ figIndex, ...newPos })

  // check is new position is overlap or on floor
  if (newCoords.some((([x, y]) => (y === boardH) || board[y][x] !== 0))) {
    // paint piece back
    for (const [x, y] of oldCoords) {
      board[y][x] = color
    }

    // remove full lines
    const boardWithoutFillLines = board.filter(line => line.some(c => c === 0))
    if (boardWithoutFillLines.length < boardH) {
      // update score
      score += scores[boardH - boardWithoutFillLines.length]

      // add new empty lines
      const newLines = []
      for (let i = 0; i < boardH - boardWithoutFillLines.length; i++) {
        newLines.push(Array(boardW).fill(0))
      }
      boardWithoutFillLines.unshift(...newLines)
      board = boardWithoutFillLines
    }

    // create new piece
    figIndex = nextFigIndex
    nextFigIndex = getRandomIntInclusive(0, figures.length - 1)
    rotateIndex = 0 //getRandomIntInclusive(0, figures[figIndex].length - 1)
    offsetX = figIndex === 0 ? 3 : 4
    offsetY = -1 * figures[figIndex][rotateIndex].ofy
    color = nextFigColor
    nextFigColor = getRandomIntInclusive(1, backColors.length - 1)

    const newCoords = getFigCoords({ figIndex, rotateIndex, offsetX, offsetY, })

    // check end of game
    if (newCoords.some((([x, y]) => board[y][x] !== 0))) {
      // console.log(newCoords)
      // console.log(newCoords)
      console.log('The End')
      process.exit()
    }

    // add new piece to board
    // for (const [x, y] of newCoords) {
    //   board[y][x] = color
    // }
  } else {
    // remove old piece
    // clearPiece({ board, figIndex, rotateIndex, offsetX, offsetY })
    // add updated piece to board
    // for (const [x, y] of newCoords) {
    //   board[y][x] = color
    // }
    ({ rotateIndex, offsetX, offsetY } = newPos)
  }

  return { move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score }
}


// const border = '\u001B[7m \u001B[m'
const Reset = '\u001b[m'
const Inverse = '\u001B[7m'
const ceil = '\u2582'
const floor = Inverse + '\u2586' + Reset
const left = Inverse + '\u258a' + Reset
const right = '\u258e'
const spacer = '.'

const renderNextPiece = (figIndex, color) => {
  const w = 6
  const h = 6
  const offsetX = figIndex === 5 ? 2 : 1
  const offsetY = figIndex === 5 ? 2 : 1
  const coords = getFigCoords({
    figIndex, rotateIndex: 0, offsetX, offsetY
  })

  const resArr = []
  let res = ''
  res += ' '
  for (let x = 0; x < w; x++) {
    res += ceil
  }
  res += ' '
  resArr.push(res)
  res = ''

  for (let y = 0; y < h; y++) {
    res += left
    for (let x = 0; x < w; x++) {
      if (coords.some(([xc, yc]) => xc === x && yc === y)) {
        res += backColors[color] + ' ' + Reset
      } else {
        res += ' '
      }
    }
    res += right
    resArr.push(res)
    res = ''
  }
  res += ' '
  for (let x = 0; x < w; x++) {
    res += floor
  }
  res += ' '
  resArr.push(res)

  return resArr
}

const render = ({ move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score }) => {
  const coords = getFigCoords({ figIndex, rotateIndex, offsetX, offsetY })
  for (const [x, y] of coords) {
    board[y][x] = color
  }

  const nP = renderNextPiece(nextFigIndex, nextFigColor)
  let res = ''
  res += ' '
  for (let x = 0; x < boardW; x++) {
    res += ceil
  }
  res += ' '
  res += '\n'

  for (let y = 0; y < boardH; y++) {
    res += left
    for (let x = 0; x < boardW; x++) {
      if (board[y][x] !== 0) {
        res += backColors[board[y][x]] + ' ' + Reset
      } else {
        res += (x % 2 === 0) ? ' ' : spacer
      }
    }
    res += right

    if (y === 0) {
      res += ' ' + String(score).padStart(6, '0')
    }

    if (y > 0 && y - 1 < nP.length) {
      res += nP[y - 1]
    }

    res += '\n'
  }
  res += ' '
  for (let x = 0; x < boardW; x++) {
    res += floor
  }
  res += ' '
  // res += '\n'

  for (const [x, y] of coords) {
    board[y][x] = 0
  }

  return res
}

const printState = ({
  move,
  board,
  figIndex,
  rotateIndex,
  color,
  offsetX,
  offsetY,
  nextFigIndex,
  nextFigColor,
  score
}) => {
  const strBoard = board.map(row => row.join('')).join('')
  console.log(move, strBoard, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score)
}

const parseState = () => {
  const move = parseInt(process.argv[2])
  const board = []
  for (let i = 0; i < process.argv[3].length; i += boardW) {
    board.push(process.argv[3].slice(i, i + boardW).split('').map(c => parseInt(c)))
  }
  // console.log(board.length)
  // console.log(board)
  const figIndex = parseInt(process.argv[4])
  const rotateIndex = parseInt(process.argv[5])
  const color = parseInt(process.argv[6])
  const offsetX = parseInt(process.argv[7])
  const offsetY = parseInt(process.argv[8])
  const nextFigIndex = parseInt(process.argv[9])
  const nextFigColor = parseInt(process.argv[10])
  const score = parseInt(process.argv[11])
  // console.log({ move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score })
  return { move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score }
}
// console.log(process.argv.length)
// console.log(process.argv.length)
// console.log(process.argv)
if (process.argv[2] === '0init') {
  const state = init()
  printState(state)
} else if (process.argv.length === 12) {
  const state = update(parseState())
  printState(state)
  console.log(render(state))
} else {
  console.log(process.argv)
  throw Error('incorrect arguments')
}
