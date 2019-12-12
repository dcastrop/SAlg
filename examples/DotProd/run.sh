#!/bin/bash

MAXCORES=$(grep ^cpu\\scores /proc/cpuinfo | uniq |  awk '{print $4}')
NCORES=${1:-"${MAXCORES}"}
NCORES=$((${NCORES}>=${MAXCORES} ? ${MAXCORES} : ${NCORES}))
NAME=t_${NCORES}

SIZES=
for i in `seq 9 30`
do
  SIZES="${SIZES} $((2 ** i))"
done

DATA_DIR=${PWD}/data
DATA=${DATA_DIR}/${NAME}
EXE=${PWD}/bench

if [ ! -d "${DATA_DIR}" ]
then
  mkdir ${DATA_DIR}
fi

echo > ${DATA}

CID=$(( ${NCORES} - 1 ))

for SIZE in ${SIZES}
do
  echo "size: ${SIZE}" >> ${DATA}
  numactl -C 0-${CID} --membind=0 ${EXE} ${SIZE} >> ${DATA}
done
