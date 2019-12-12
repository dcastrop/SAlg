#!/bin/zsh

DIR=$1

plotfn() {
  SZ=$1
  NAME=$DIR:l

  PATH=${DIR}/data/t_48_${1}
  OUT="${NAME}_$SZ"

  echo "Plotting ${PATH} as ${OUT}_[s|k].pdf"
  ./plot.py ${1} plots/${OUT} ${PATH}
}

plotfn 12
plotfn 24
plotfn 48

