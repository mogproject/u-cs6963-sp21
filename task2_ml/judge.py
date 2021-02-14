#!/usr/bin/env python3
"""
Santorini judge.
"""

__author__ = 'Yosuke Mizutani'
__version__ = '0.0.1'
__license__ = 'Apache License, Version 2.0'

# imports standard libraries
import sys
import argparse
import logging

def get_logger(log_level=logging.CRITICAL + 1):
    """Logger settings."""

    logging.basicConfig(
        level=log_level,
        format="%(asctime)s [%(levelname)s] %(message)s",
        stream=sys.stderr)
    logger = logging.getLogger(__name__)
    return logger

def get_parser():
    """Argument parser."""

    parser = argparse.ArgumentParser(description='Santorini judge')
    parser.add_argument('player1')
    parser.add_argument('player2')
    parser.add_argument('--log-level', choices=['crit', 'error', 'warn', 'info', 'debug', 'none'], default='info', help='log level')
    return parser


def main(args):
    """Entry point of the program. """

    # get logger
    logger = get_logger({'crit': logging.CRITICAL, 'error': logging.ERROR, 'warn': logging.WARNING, 'info': logging.INFO, 'debug': logging.DEBUG, 'none': logging.CRITICAL + 1}[
        args.log_level])

    # open player programs

    
    print(args)


if __name__ == '__main__':
    main(get_parser().parse_args())
