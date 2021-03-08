#!/usr/bin/env python3
"""
Splits tournament results.
"""

from pathlib import Path
import argparse


def get_parser():
    """Argument parser."""

    parser = argparse.ArgumentParser(description='Summarizes tournament results.')
    parser.add_argument('prefix', help='prefix of the tournament log')
    return parser


def output(prefix, i, game):
    path = f'{prefix}/{i:03d}.log'  # not compatible with Windows
    print(f'Writing: {path}')
    with open(path, 'w') as f:
        f.write('\n'.join(game))


def main(args):
    """Entry point of the program. """

    if not Path(args.prefix).exists():
        print(f'Creating directory: {args.prefix}')
        Path(args.prefix).mkdir()

    with open(args.prefix + '.log') as f:
        i = 0
        game = []
        for line in f:
            line = line.strip()
            if '"turn":0' in line:
                if i >= 1:
                    output(args.prefix, i, game)
                i += 1
                game = [line]
            else:
                game += [line]
        output(args.prefix, i, game)


if __name__ == '__main__':
    main(get_parser().parse_args())
