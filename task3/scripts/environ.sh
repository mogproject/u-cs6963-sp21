# Usage:
#   - Use zsh
#   - Run: source scripts/environ.sh

SCRIPT_DIR=$( cd "$( dirname "$0" )" 1>/dev/null && pwd -P )
PROJECT_ROOT=$( dirname "${SCRIPT_DIR}" )
JUDGE_DIR="${PROJECT_ROOT}/provided/santorini-cards-mac/bin"

export rn="${JUDGE_DIR}/run"
export gu="${JUDGE_DIR}/gui"
export tn="${JUDGE_DIR}/tournament"
export pr="${JUDGE_DIR}/play-rate"
export pl="${PROJECT_ROOT}/.stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/santorini-player/santorini-player"

# cards
export c1=Apollo
export c2=Artemis
export c3=Atlas
export c4=Demeter
export c5=Hephastus
export c6=Minotaur
export c7=Pan
export c8=Prometheus
