#!/usr/bin/env python3
"""
Santorini benchmark launcher.
"""

__author__ = 'Yosuke Mizutani'
__version__ = '0.0.2'
__license__ = 'Apache License, Version 2.0'

import sys
import argparse
import statistics
import subprocess
import re
from collections import defaultdict

DEFAULT_NUM_GAMES = 10
REG_MOVE = re.compile(r'^Move (\d)')
REG_TIME = re.compile(r'^in ([\d.]+) secs')
REG_WIN = re.compile(r'^Win by (\d)')
REG_NO_MOVE = re.compile(r'^No move possible for (\d)')
REG_BAD_MOVE = re.compile(r'^Bad move by (\d)')
REG_TIMEOUT = re.compile(r'^Error .* from (\d)')


def get_parser():
    """Argument parser."""

    parser = argparse.ArgumentParser(description='Santorini benchmark launcher.')
    parser.add_argument('judge_exe', help='path to the judge program')
    parser.add_argument('player1_exe', help='path to the first player program')
    parser.add_argument('player2_exe', help='path to the second player program')
    parser.add_argument('-n', '--num-games', type=int, default=DEFAULT_NUM_GAMES, help=f'number of games (default:{DEFAULT_NUM_GAMES})')
    parser.add_argument('--timeout', type=int, help='move timeout in seconds (default:None)')
    return parser


def parse_output(line):
    """Parses output lines."""

    for i, r in enumerate([REG_MOVE, REG_TIME, REG_WIN, REG_NO_MOVE, REG_BAD_MOVE, REG_TIMEOUT]):
        m = r.match(line)
        if m:
            return i, float(m.group(1)) if i == 1 else int(m.group(1)) - 1
    return -1, None


def main(args):
    """Entry point of the program. """

    print(f'Player 1: {args.player1_exe}')
    print(f'Player 2: {args.player2_exe}')

    cmd = [args.judge_exe, args.player1_exe, args.player2_exe]
    if args.timeout is not None:
        cmd += ['--timeout', args.timeout]

    time_info = [[], []]
    winners = []
    reasons = [defaultdict(int), defaultdict(int)]

    for _ in range(args.num_games):
        out = subprocess.check_output(cmd, encoding='utf8', stderr=subprocess.STDOUT)

        current_player = 0

        for line in out.splitlines():
            t, x = parse_output(line)

            if t == 0:  # move
                current_player = x
            elif t == 1:  # time
                time_info[current_player] += [x]
            elif t == 2:  # win
                winners += [x]
                reasons[x][0] += 1
                break
            elif 3 <= t <= 5:
                winners += [1 - x]
                reasons[x][t - 2] += 1
                break

        sys.stdout.write('ox'[winners[-1]])
        sys.stdout.flush()

    # show stats
    p2_win = sum(winners)
    p1_win = len(winners) - p2_win

    reason_table = ['Win by move', 'No move possible', 'Bad move', 'Timeout']
    reason_text = [', '.join(f'{reason_table[k]}: {v}' for k, v in reasons[p].items()) for p in range(2)]

    print('\n')
    print('Win Rate:')
    print(f'  Player 1: {100.0 * p1_win / len(winners):6.2f}%    -- {reason_text[0]}')
    print(f'  Player 2: {100.0 * p2_win / len(winners):6.2f}%    -- {reason_text[1]}')
    print()

    for p in range(2):
        pstdev = statistics.pstdev(time_info[p])
        min_val = min(time_info[p])
        max_val = max(time_info[p])
        mean = statistics.fmean(time_info[p])
        quant = statistics.quantiles(time_info[p])

        print(f'Player {p + 1} Time (sec):')
        print(f'  Mean: {mean:.6f}, StdDev: {pstdev:.6f}')
        print(f'  Min: {min_val:.6f}, 1st Quantile: {quant[0]:.6f}, Median: {quant[1]:.6f}, 3rd Quantile: {quant[0]:.6f}, Max: {max_val:.6f}')
        print()


if __name__ == '__main__':
    main(get_parser().parse_args())
