#!/usr/bin/env python3

import gamestate as gs
from search import search, searchAlphaBeta

EX = [
    '{"turn":18,"spaces":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"players":[[[2,3],[4,4]],[[2,5],[3,5]]]}',
    '{"turn":0,"spaces":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[1,1],[2,2]],[[4,5],[5,5]]]}',
    '{"turn":0,"spaces":[[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[1,1],[2,2]],[[2,3],[3,2]]]}',
    '{"turn":17,"spaces":[[0,0,0,0,1],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],"players":[[[2,3],[4,4]],[[2,4],[3,5]]]}',
    '{"turn":18,"spaces":[[0,0,0,0,2],[0,1,2,1,0],[1,0,0,3,0],[0,0,3,0,2],[0,0,0,0,3]],"players":[[[1,3],[4,4]],[[2,5],[3,5]]]}',
    '{"turn":10,"spaces":[[0,0,0,2,0],[0,0,0,2,1],[0,0,0,0,0],[1,0,0,2,0],[1,0,1,0,0]],"players":[[[2,1],[4,4]],[[1,5],[5,1]]]}',
]


def runGame(initialState):
    print('=' * 80)
    s = initialState
    print(s)

    while s.score not in [-gs.WIN, gs.WIN]:
        #mv, _ = search(s)
        score, moves, stats = searchAlphaBeta(s, 3)
        print(score, moves)
        s = gs.makeMove(s, moves[0])
        print(s)


def f1():
    s = gs.fromJSON(EX[1])
    states = [gs.makeMove(s, m) for m in s.legalMoves]

    for state in sorted(states)[-5:]:
        print(state)

    s = gs.fromJSON(EX[0])
    states = [gs.makeMove(s, m) for m in s.legalMoves]

    for state in sorted(states)[-5:]:
        print(state)

    runGame(gs.fromJSON(EX[1]))
    runGame(gs.fromJSON(EX[2]))
    runGame(gs.fromJSON(EX[3]))

def f2():
    s = gs.fromJSON(EX[1])
    print(s)
    score, moves, stats = searchAlphaBeta(s, 2)
    print(score, moves, stats)
    print(gs.makeMove(s, moves[0]))

def f3():
    # must win next turn: move (1,3) -> (1,4); build (2,4)
    runGame(gs.fromJSON(EX[4]))
    runGame(gs.fromJSON(EX[5]))

def assertScoreLessThan(s1, s2, description):
    st1, st2 = gs.fromJSON(s1), gs.fromJSON(s2)
    sc1, sc2 = st1.score, st2.score

    # print(st2)
    # print(st1)
    assert sc1 < sc2, f'not {sc1} < {sc2}: {description}\n{st1}\n{st2}'

def test_scoring():
    s1 = '{"turn":1,"spaces":[[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[1,2],[2,2]],[[4,5],[5,5]]]}'
    s2 = '{"turn":1,"spaces":[[0,0,0,0,0],[1,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[1,2],[2,2]],[[4,5],[5,5]]]}'
    s3 = '{"turn":1,"spaces":[[0,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[2,1],[2,2]],[[4,5],[5,5]]]}'
    assertScoreLessThan(s2, s1, 'should build farther from opponent')
    assertScoreLessThan(s3, s1, 'should build farther from opponent')

    s11 = '{"turn":8,"spaces":[[0,0,2,0,0],[0,0,2,2,2],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[1,4],[1,5]],[[3,3],[3,4]]]}'
    s12 = '{"turn":9,"spaces":[[0,0,2,0,0],[1,0,2,2,2],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[3,3],[3,4]],[[1,4],[1,5]]]}'
    assert gs.fromJSON(s11).score == -gs.WIN
    assert gs.fromJSON(s12).score == gs.WIN

    s21 = '{"turn":3,"spaces":[[1,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,1]],"players":[[[1,1],[2,2]],[[4,4],[4,5]]]}'
    s22 = '{"turn":3,"spaces":[[1,0,0,0,0],[0,1,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,1]],"players":[[[2,2],[3,3]],[[4,4],[4,5]]]}'
    assertScoreLessThan(s22, s21, 'should build next to the boundary')

    s31 = '{"turn":2,"spaces":[[1,0,0,0,0],[0,0,0,0,0],[0,0,0,1,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[2,1],[2,2]],[[2,3],[3,3]]]}'
    s32 = '{"turn":2,"spaces":[[2,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]],"players":[[[2,1],[2,2]],[[1,2],[3,2]]]}'
    assertScoreLessThan(s32, s31, 'Player 2 should prevent Player 1 from climbing up')


    print('ok')

test_scoring()
# f1()
