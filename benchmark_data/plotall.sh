#!/bin/bash

DIR=$1
CORES=$2

plotfn() {
  SZ=$1
  NAME=${DIR,,}

  PATH=${DIR}/data/t_48_${1}
  OUT="${NAME}_$SZ"

  echo "Plotting ${PATH} as ${OUT}_[s|k].pdf"
  ./plot.py ${1} plots/${OUT} ${PATH}
}

plotfn ${CORES}

