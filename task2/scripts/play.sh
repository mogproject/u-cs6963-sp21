#!/bin/bash

SCRIPT_DIR=$( cd "$( dirname "$0" )" && pwd -P )
PROJECT_ROOT=$( dirname "${SCRIPT_DIR}" )
JUDGE_DIR="${PROJECT_ROOT}/provided/santorini-mac/bin"
MY_PLAYER="${PROJECT_ROOT}/.stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/santorini-player/santorini-player"

usage() {
    echo "Usage: $0 <option>" 
    echo ""
    echo "  option:"
    echo "    1: santorini-player vs santorini-player"
    echo "    2: santorini-player vs play-search"
    echo "    3: play-search vs santorini-player"
    echo "    4: santorini-player vs play-random"
    echo "    5: play-random vs santorini-player"

    exit 1
}

if [[ $# -ne 1 ]]; then usage; fi

case $1 in
    1) p1=${MY_PLAYER}; p2=${MY_PLAYER};;
    2) p1=${MY_PLAYER}; p2=${JUDGE_DIR}/play-search;;
    3) p1=${JUDGE_DIR}/play-search; p2=${MY_PLAYER};;
    4) p1=${MY_PLAYER}; p2=${JUDGE_DIR}/play-random;;
    5) p1=${JUDGE_DIR}/play-random; p2=${MY_PLAYER};;
    *) usage;;
esac

# start
${JUDGE_DIR}/run ${p1} ${p2} | ${JUDGE_DIR}/gui
