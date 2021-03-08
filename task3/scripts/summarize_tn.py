#!/usr/bin/env python3
"""
Summarizes tournament results.
"""

import sys
import argparse
import re
from collections import defaultdict


REG_START = re.compile(r'^Playing 1:[^ ]+ as ([^\s]+) vs. 2:[^ ]+ as ([^\s]+)')
REG_WIN = re.compile(r'^Win by (\d)')
REG_NO_MOVE = re.compile(r'^No move possible for (\d)')
REG_BAD_MOVE = re.compile(r'^Bad move by (\d)')
REG_TIMEOUT = re.compile(r'^Error .* from (\d)')


def get_parser():
    """Argument parser."""

    parser = argparse.ArgumentParser(description='Summarizes tournament results.')
    return parser


def check(regs, line):
    for reg in regs:
        m = reg.match(line)
        if m:
            return m.group(1)
    return None


def output(i, setup, result):
    entries = [f'{i:03d}', setup[0], setup[1], 'WIN' if result[0] else 'LOSE', result[1]]
    print(' '.join(f'{s:12s}' for s in entries))

def main(args):
    """Entry point of the program. """

    i = 0
    setup = None
    result = None

    for line in sys.stdin:
        line = line.strip()
        start = REG_START.match(line)
        if start:
            if i >= 1:
                output(i, setup, result)
            i += 1
            setup = (start.group(1), start.group(2))
            continue
        win = check([REG_WIN], line)
        if win is not None:
            # program 1 wins?
            result = ((i % 2 == 0) ^ (win == '1'), line)
            continue
        lose = check([REG_NO_MOVE, REG_BAD_MOVE, REG_TIMEOUT], line)
        if lose is not None:
            result = ((i % 2 == 0) ^ (lose == '2'), line)
            continue
    output(i, setup, result)


if __name__ == '__main__':
    main(get_parser().parse_args())
