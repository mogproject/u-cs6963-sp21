#!/bin/bash

SCRIPT_DIR=$( cd "$( dirname "$0" )" && pwd -P )
source "${SCRIPT_DIR}/environ.sh"

$rn $pr $pl | $gu
$rn --cards $c1 $c2 $pr $pl | $gu
