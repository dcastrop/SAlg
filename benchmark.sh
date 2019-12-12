#!/bin/bash

sbuild () {
  echo "+ stack exec -- session-arrc $1.hs"
  stack exec -- session-arrc $1.hs
  echo "+ gcc $1.c main.c -pthread -lm -o bench"
  gcc $1.c main.c -pthread -lm -o bench
}

REPETITIONS=${1:-"50"}

echo "REPETITIONS=${REPETITIONS}"

DIRS="DotProd FFT Mergesort Quicksort ScalarMulMat"

pushd examples

for dir in ${DIRS}
do
  pushd ./${dir}
  GT=$(sbuild ${dir} | tee /dev/tty | sed -n 5p)
  echo "# Generating global type for: ${GT}"
  echo "+ stack exec -- session-arrc --infer ${GT} ${dir}.hs"
  stack exec -- session-arrc --infer ${GT} ${dir}.hs
  popd
done

popd

