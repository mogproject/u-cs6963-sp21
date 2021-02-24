import copy
import json
from typing import List, Tuple
from collections import deque

rd = [-1, -1, 0, 1, 1, 1, 0, -1]
rc = [0, 1, 1, 1, 0, -1, -1, -1]
WIN = 1e9

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
    '{"turn":0,"spaces":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[1,1],[2,2]],[[2,3],[3,2]]]}',
]
DIST_UNREACHABLE = 100


class GameState:
    def __init__(self, players, levels=[0 for _ in range(25)], turn=0, lastMove=-1, lastBuild=-1, computeScore=True) -> None:
        self.players = players
        self.levels = levels
        self.turn = turn
        self.lastMove = lastMove
        self.lastBuild = lastBuild
        self.available = [self.levels[i] < 4 for i in range(25)]

        if computeScore:
            # occupied
            for workers in self.players:
                for index in workers:
                    self.available[index] = False

            # compute reachability
            self.bfs()

            # legal moves
            self.legalMoves = legalMoves(self)

            # score
            self.score = evaluate(self)

    def __lt__(self, other):
        if isinstance(other, GameState):
            return self.score < other.score

    def isFinished(self):
        return self.score in [WIN, -WIN]

    def bfs(self):
        self.dist = [[[DIST_UNREACHABLE for _ in range(25)] for _ in range(2)] for _ in range(2)]

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

                        if 0 <= nbr and self.dist[player][worker][nbr] == DIST_UNREACHABLE and avail[nbr] and self.levels[nbr] <= self.levels[p] + 1:
                            self.dist[player][worker][nbr] = self.dist[player][worker][p] + 1
                            q.append(nbr)

    def __repr__(self) -> str:
        workers = [' ' for _ in range(25)]
        workers[self.players[0][0]] = 'A'
        workers[self.players[0][1]] = 'B'
        workers[self.players[1][0]] = 'x'
        workers[self.players[1][1]] = 'y'

        buf = []
        buf += [f'Turn: {self.turn}, Score: {self.score:.1f}']
        for r in range(1, 6):
            row = ''
            for c in range(1, 6):
                index = encodeIndex(r, c)
                s = f'{workers[index]}{self.levels[index]}'
                s = f'[{s}]' if index in [self.lastMove, self.lastBuild] else f' {s} '
                row += s
            buf += [row]
        return '\n'.join(buf)


# EVAL_PROXIMITY = 50
# EVAL_FIRST_ASCENT = 300
# EVAL_HIDEOUT = 10
# EVAL_LEVELING = 1000
EVAL_PROX_TABLE = [0, 100, 80, 50, 20, 0]
EVAL_REACH_TABLE = [
    [  # turn to move advantages
        [10, 9, 8, 7, 6, 5, 4, 0],  # level 0
        [300, 200, 40, 30, 20, 15, 12, 0],  # level 1
        [10000, 2000, 500, 200, 150, 120, 110, 0],  # level 2
        [WIN, 20000, 5000, 2000, 1000, 500, 300, 0],  # level 3
        [0, 0, 0, 0, 0, 0, 0, 0],  # level 4
    ],
    [  # not turn to move
        [10, 9, 8, 7, 6, 5, 4, 0],  # level 0
        [300, 100, 40, 30, 20, 15, 12, 0],  # level 1
        [10000, 1000, 500, 200, 150, 120, 110, 0],  # level 2
        [WIN, 10000, 5000, 2000, 1000, 500, 300, 0],  # level 3
        [0, 0, 0, 0, 0, 0, 0, 0],  # level 4
    ],
]
EVAL_ASYM_TABLE = [
    [0, 0, 0, 1, 2, 3, 4, 5, 10],  # level 0
    [0, 0, 0, 10, 20, 30, 40, 50, 100],  # level 1
    [0, 100, 1000, 2000, 2000, 2000, 2000, 2000, 2000],  # level 2
    [0, 1000, 20000, 30000, 30000, 30000, 30000, 30000, 30000],  # level 3
    [0, 0, 0, 0, 0, 0, 0, 0],  # level 4
]
EVAL_CORNER_BONUS = 20
EVAL_STUCK_BONUS = 25000
EVAL_PREVENTION_ADVANTAGE = 20000


def sign(player: int) -> float:
    return [1.0, -1.0][player]


def isStuck(state: GameState, player: int, worker: int) -> bool:
    index = state.players[player][worker]
    return all(nbr == -1 or state.levels[nbr] > state.levels[index] + 1 for nbr in neighborTable[index])


def evaluate(state: GameState) -> float:
    currentPlayer = state.turn % 2
    currentOpponent = 1 - currentPlayer

    if not state.legalMoves:
        return sign(currentOpponent) * WIN

    ret = [[0, 0, 0, 0, 0], [0, 0, 0, 0, 0]]

    for p in range(2):
        for w in range(2):
            if state.levels[state.players[p][w]] == 3:
                return sign(p) * WIN

    # firstAscentEligible = sum(state.levels[x] for ws in state.players for x in ws) == 0

    # 1. Worker Proximity
    prox = [[state.dist[p][w][state.players[p][1 - w]] for w in range(2)] for p in range(2)]  # [[p1w1 -> p1w2, p1w2 -> p1w1], [p2w1 -> p2w2, p2w2 -> p2w1]]
    for p in range(2):
        ret[p][0] = sum(EVAL_PROX_TABLE[min(len(EVAL_PROX_TABLE) - 1, prox[p][w])] for w in range(2))

    # 2. Reachability
    dist = [[min(state.dist[p][j][i] for j in range(2)) for i in range(25)] for p in range(2)]  # shortest distance from any worker
    for p in range(2):
        ret[p][1] = sum(EVAL_REACH_TABLE[p ^ currentPlayer][state.levels[i]][min(len(EVAL_REACH_TABLE[0][0]) - 1, dist[p][i])] for i in range(25))

    # 3. Asymmetry
    for p in range(2):
        for i in range(25):
            if dist[p][i] < dist[1 - p][i]:
                # corner bonus
                cnt = sum(1 if neighborTable[i][d] == -1 or not state.available[neighborTable[i][d]] else 0 for d in range(8))
                ret[p][2] += cnt * EVAL_CORNER_BONUS * state.levels[i]
                ret[p][2] += EVAL_ASYM_TABLE[state.levels[i]][min(len(EVAL_ASYM_TABLE[0]) - 1, dist[1 - p][i] - dist[p][i])]

    # 4. Stuck Bonus
    for p in range(2):
        for w in range(2):
            if isStuck(state, p, w):
                # one worker is stuck
                ret[1 - p][3] = EVAL_STUCK_BONUS

    # 5. Prevention
    for p in range(2):
        for w in range(2):
            index = state.players[p][w]
            if state.levels[index] == 2:  # worker at level 2
                best = DIST_UNREACHABLE
                for nbr in (neighborTable[index][d] for d in range(8)):
                    best = min(best, DIST_UNREACHABLE if nbr == -1 else dist[1 - p][nbr])
                if best >= 2:  # opponent is too far from this worker
                    ret[p][4] += EVAL_PREVENTION_ADVANTAGE

    # print(dist)
    # score details
    # print(ret)
    return sum(ret[0]) - sum(ret[1])

    # for player in range(2):
    #     sgn = sign(player)

    #     # 1. Proximity
    #     p1 = state.dist[player][0][state.players[player][1]]  # worker1 -> worker2
    #     p2 = state.dist[player][1][state.players[player][0]]  # worker2 -> worker1

    #     ret += sgn * EVAL_PROXIMITY / (p1 ** 2) if p1 > 0 else 0
    #     ret += sgn * EVAL_PROXIMITY / (p2 ** 2) if p2 > 0 else 0

    #     # 2. Reachability
    #     for index in range(25):
    #         lv = state.levels[index]
    #         if lv == 4:
    #             continue

    #         base = [1, 100, 3000, 10000][lv]
    #         for worker in range(2):
    #             d = state.dist[player][worker][index]
    #             if d == DIST_UNREACHABLE:
    #                 continue  # unreachable
    #             if lv == 3 and d == 0:
    #                 return sign(player) * WIN
    #             if firstAscentEligible and lv == 1 and d == 1 and player == currentPlayer:
    #                 ret += sign(currentPlayer) * EVAL_FIRST_ASCENT
    #                 firstAscentEligible = False

    #             factor = 1 if d == 0 else (d + 1) ** 2
    #             ret += sgn * (base / factor)

    # if state.lastBuild != 0:
    #     # Hideout Bonus
    #     d = min(state.dist[currentPlayer][w][state.lastBuild] for w in range(2))
    #     if d >= 3:
    #         ret += sign(currentOpponent) * EVAL_HIDEOUT * (min(10, d) ** 3)

    #     # Leveling Bonus
    #     if state.levels[state.lastBuild] >= 2 and state.levels[state.lastMove] <= state.levels[state.lastBuild] <= state.levels[state.lastMove] + 1:
    #         ret += sign(currentOpponent) * EVAL_LEVELING * state.levels[state.lastBuild]

    # return ret


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
