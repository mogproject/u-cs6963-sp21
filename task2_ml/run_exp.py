#!/usr/bin/env python3

import gamestate as gs
from search import search

s = gs.fromJSON(gs.EX[1])
moves = gs.legalMoves(s)
states = [gs.makeMove(s, m) for m in moves]
scores = [gs.evaluate(ss) for ss in states]

for state in sorted(states):
    print(state)
