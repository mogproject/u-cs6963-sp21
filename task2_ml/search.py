import gamestate as gs
from typing import List, Tuple


def search(state: gs.GameState) -> Tuple[int, float]:
    playerSign = 1 if state.turn % 2 == 0 else -1
    candidates = gs.legalMoves(state)
    if not candidates:
        return (-1, -1e10 * playerSign)

    fst = gs.makeMove(state, candidates[0])
    bestScore = gs.evaluate(fst) * playerSign
    bestCand = 0

    for i in range(1, len(candidates)):
        st = gs.makeMove(state, candidates[i])
        score = gs.evaluate(st)
        if score * playerSign > bestScore:
            bestScore = score * playerSign
            bestCand = i

    return candidates[bestCand], bestScore
