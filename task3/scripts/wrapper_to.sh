#!/bin/bash

SCRIPT_DIR=$( cd "$( dirname "$0" )" 1>/dev/null && pwd -P )
source $SCRIPT_DIR/environ.sh

$pl --timeout 10

