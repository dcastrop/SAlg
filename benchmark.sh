#!/bin/bash

me=`basename "$0"`

sbuild () {
  echo "+ stack exec -- session-arrc $1.hs"
  stack exec -- session-arrc $1.hs
  echo "+ gcc $1.c main.c -DREPETITIONS=${REPETITIONS} -pthread -lm -o bench"
  gcc $1.c main.c -DREPETITIONS=${REPETITIONS} -pthread -lm -o bench
}

MAXCORES=$(grep ^cpu\\scores /proc/cpuinfo | uniq |  awk '{print $4}')
NCORES=${1:-"${MAXCORES}"}
NCORES=$((${NCORES}>=${MAXCORES} ? ${MAXCORES} : ${NCORES}))

REPETITIONS=${2:-"50"}

echo "# Running ./${me} CORES REPETITIONS"
echo "#    with CORES       = ${NCORES}"
echo "#     and REPETITIONS = ${REPETITIONS}"

DIRS="DotProd FFT Mergesort Quicksort ScalarMulMat"

for dir in ${DIRS}
do
  pushd ./examples/${dir} > /dev/null
  echo "+ pushd ./examples/${dir}"
  GT=$(sbuild ${dir} | tee /dev/tty | sed -n 5p)
  echo "# Generating global type for: ${GT}"
  echo "+ stack exec -- session-arrc --infer ${GT} ${dir}.hs"
  stack exec -- session-arrc --infer ${GT} ${dir}.hs
  cat ${dir}_${GT}.mpst
  ./run.sh ${NCORES}
  popd > /dev/null
done

