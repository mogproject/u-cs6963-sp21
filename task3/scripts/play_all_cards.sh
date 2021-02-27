#!/bin/bash

SCRIPT_DIR=$( cd "$( dirname "$0" )" && pwd -P )
PROJECT_ROOT=$( dirname "${SCRIPT_DIR}" )
JUDGE_DIR="${PROJECT_ROOT}/provided/santorini-cards-mac/bin"
MY_PLAYER="${PROJECT_ROOT}/.stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/santorini-player/santorini-player"

#${JUDGE_DIR}/run ${JUDGE_DIR}/play-rate ${MY_PLAYER} | ${JUDGE_DIR}/gui
#${JUDGE_DIR}/run --cards Apollo Artemis ${JUDGE_DIR}/play-rate ${MY_PLAYER} | ${JUDGE_DIR}/gui
#${JUDGE_DIR}/run --cards Atlas Demeter ${JUDGE_DIR}/play-rate ${MY_PLAYER} | ${JUDGE_DIR}/gui
${JUDGE_DIR}/run --cards Hephastus Minotaur ${JUDGE_DIR}/play-rate ${MY_PLAYER} | ${JUDGE_DIR}/gui
#${JUDGE_DIR}/run --cards Pan Prometheus ${JUDGE_DIR}/play-rate ${MY_PLAYER} | ${JUDGE_DIR}/gui
