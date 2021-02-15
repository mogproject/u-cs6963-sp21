import gamestate as gs
from typing import Dict, List, Tuple

INF = gs.WIN + 1


def search(state: gs.GameState) -> Tuple[int, float]:
    playerSign = 1 if state.turn % 2 == 0 else -1

    if not state.legalMoves:
        return (-1, -gs.WIN * playerSign)

    fst = gs.makeMove(state, state.legalMoves[0])
    bestScore = fst.score * playerSign
    bestCand = 0

    for i in range(1, len(state.legalMoves)):
        st = gs.makeMove(state, state.legalMoves[i])

        if st.score * playerSign > bestScore:
            bestScore = st.score * playerSign
            bestCand = i

    return state.legalMoves[bestCand], bestScore


def searchAlphaBeta(state: gs.GameState, depth: int) -> Tuple[float, List[int], Dict]:
    stats = {'n': 0, 'a': 0, 'l': 0}
    moves = [-1 for _ in range(depth)]
    score = searchAlphaBetaRec(state, depth, min(depth, 2), -INF, INF, stats, moves)
    return score, moves, stats


def searchAlphaBetaRec(
        state: gs.GameState,
        depth: int,
        fullScanDepth: int,
        alpha: float,
        beta: float,
        stats: Dict,
        moves: List[int]
) -> Tuple[int, float]:
    stats['n'] += 1  # searched node count

    if depth == 0 or state.isFinished():
        stats['l'] += 1
        return state.score

    keepMax = state.turn % 2 == 0

    nextStates = sorted(((gs.makeMove(state, mv), mv) for mv in state.legalMoves), reverse=keepMax)
    if fullScanDepth <= 0:
        nextStates = nextStates[:5]

    best = -INF if keepMax else INF
    bestMove = -1
    for child, mv in nextStates:
        score = searchAlphaBetaRec(child, depth - 1, fullScanDepth - 1, alpha, beta, stats, moves)
        if (best < score and keepMax) or (best > score and not keepMax) :
            best = score
            bestMove = mv
            if keepMax:
                alpha = max(alpha, score)
            else:
                beta = min(beta, score)

            if beta <= alpha:
                stats['a'] += 1
                break  # beta-alpha cutoff
    assert gs.isLegal(state, bestMove), str(bestMove) + ' ' + gs.printMove(state, bestMove)

    moves[-depth] = bestMove
    return best
