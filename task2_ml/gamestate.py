import copy
import json
from typing import List, Tuple
from collections import deque

rd = [-1, -1, 0, 1, 1, 1, 0, -1]
rc = [0, 1, 1, 1, 0, -1, -1, -1]

neighborTable = []

for r in range(5):
    for c in range(5):
        xs = []
        for d in range(8):
            rr, cc = r + rd[d], c + rc[d]
            xs += [rr * 5 + cc if 0 <= rr <= 4 and 0 <= cc <= 4 else -1]
        neighborTable += [xs]

EX = [
    '{"turn":18,"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"players":[[[2,3],[4,4]],[[2,5],[3,5]]]}',
    '{"turn":0,"spaces":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[1,1],[2,2]],[[4,5],[5,5]]]}',
]


class GameState:
    def __init__(self, players, levels=[0 for _ in range(25)], turn=0, lastMove=-1, lastBuild=-1) -> None:
        self.players = players
        self.levels = levels
        self.turn = turn
        self.lastMove = lastMove
        self.lastBuild = lastBuild
        self.available = [self.levels[i] < 4 for i in range(25)]

        # occupied
        for workers in self.players:
            for index in workers:
                self.available[index] = False

        # compute reachability
        self.bfs()

        # score
        self.score = evaluate(self)

    def __lt__(self, other):
        if isinstance(other, GameState):
            return self.score < other.score

    def bfs(self):
        self.dist = [[[-1 for _ in range(25)] for _ in range(2)] for _ in range(2)]

        for player in range(2):
            avail = copy.deepcopy(self.available)
            for worker in range(2):
                avail[self.players[player][worker]] = True

            for worker in range(2):
                start = self.players[player][worker]
                q = deque()
                q.append(start)
                self.dist[player][worker][start] = 0

                while q:
                    p = q.popleft()

                    for d in range(8):
                        nbr = neighborTable[p][d]

                        if 0 <= nbr and self.dist[player][worker][nbr] == -1 and avail[nbr] and self.levels[nbr] <= self.levels[p] + 1:
                            self.dist[player][worker][nbr] = self.dist[player][worker][p] + 1
                            q.append(nbr)

    def __repr__(self) -> str:
        workers = [' ' for _ in range(25)]
        workers[self.players[0][0]] = 'A'
        workers[self.players[0][1]] = 'B'
        workers[self.players[1][0]] = 'x'
        workers[self.players[1][1]] = 'y'

        buf = []
        buf += [f'Turn: {self.turn}, Score: {self.score:.6f}']
        for r in range(1, 6):
            row = ''
            for c in range(1, 6):
                index = encodeIndex(r, c)
                s = f'{workers[index]}{self.levels[index]}'
                s = f'[{s}]' if index in [self.lastMove, self.lastBuild] else f' {s} '
                row += s
            buf += [row]
        return '\n'.join(buf)


def evaluate(state: GameState) -> float:
    ret = 0
    for player in range(2):
        sgn = 1 if player == 0 else -1

        # 1. Proximity
        p1 = state.dist[player][0][state.players[player][1]]  # worker1 -> worker2
        p2 = state.dist[player][1][state.players[player][0]]  # worker2 -> worker1

        ret += sgn * ((p1 + 1) ** 2 + (p2 + 1) ** 2) * 100

        # 2. Reachability
        for index in range(25):
            lv = state.levels[index]
            if lv == 4:
                continue

            base = [1, 300, 500, 10000][lv]
            for worker in range(2):
                d = state.dist[player][worker][index]
                if d < 0:
                    continue
                factor = (d + 1) ** 2
                ret += sgn * (base / factor)
    return ret


def legalMoves(state: GameState) -> List[int]:
    return [i for i in range(0, 128) if isLegal(state, i)]


def printBoard(board: List[int]) -> str:
    return '\n'.join(' '.join('%2s' % x for x in board[r * 5: (r + 1) * 5]) for r in range(5))


def printMove(state: GameState, move: int) -> str:
    _, moveFrom, moveTo, buildIndex = decodeMove(state, move)
    return f'{decodeIndex(moveFrom)} -> {decodeIndex(moveTo)}: {decodeIndex(buildIndex)}'


def encodeIndex(r: int, c: int) -> int:
    return (r - 1) * 5 + (c - 1)


def decodeIndex(index: int) -> Tuple[int, int]:
    return (index // 5) + 1, (index % 5) + 1


def encodeMove(worker: int, moveDir: int, buildDir: int) -> int:
    """
    worker  : [0,1]
    moveDir : [0,7]
    buildDir: [0,7]
    """
    return moveDir + buildDir * 8 + worker * 64


def decodeMove(state: GameState, move: int) -> Tuple[int, int, int, int]:
    worker = move // 64
    moveDir = move % 8
    buildDir = move % 64 // 8

    moveFrom = state.players[state.turn % 2][worker]
    moveTo = neighborTable[moveFrom][moveDir]
    if moveTo < 0:  # out of board
        return worker, moveFrom, -1, -1
    buildIndex = neighborTable[moveTo][buildDir]
    return worker, moveFrom, moveTo, buildIndex


def fromJSON(text: str) -> GameState:
    obj = json.loads(text)
    players = [[encodeIndex(a, b), encodeIndex(c, d)] for ((a, b), (c, d)) in obj['players']]
    levels = [x for xs in obj['spaces'] for x in xs]
    return GameState(players, levels, obj['turn'])


def makeMove(state: GameState, move: int) -> GameState:
    worker, _, moveTo, buildIndex = decodeMove(state, move)
    ps = copy.deepcopy(state.players)
    ls = copy.deepcopy(state.levels)
    ps[state.turn % 2][worker] = moveTo

    if state.levels[moveTo] < 3:
        ls[buildIndex] += 1  # build only when the game continues
    return GameState(ps, ls, state.turn + 1, moveTo, buildIndex)


def isLegal(state: GameState, move: int) -> bool:
    worker, moveFrom, moveTo, buildIndex = decodeMove(state, move)
    if buildIndex < 0:  # out of board
        return False

    if not state.available[moveTo]:  # occupied
        return False
    if buildIndex != moveFrom and not state.available[buildIndex]:  # occupied
        return False
    if state.levels[moveFrom] + 1 < state.levels[moveTo]:  # too high
        return False
    return True


def getNeighbor(index: int, direction: int) -> int:
    return neighborTable[index][direction]
