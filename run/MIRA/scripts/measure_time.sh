#!/bin/bash

NAME_SELF="measure_time"

DIR_SCRIPT="scripts"
DIR_LOG="log"
FILE_PYENV=".venv/bin/activate"
FILE_PYTHON_SCRIPT="src/remap.py"
FILE_SUBMISSION_HISTORY="log/history.txt"
FILE_SIF="/data10/imageshare/cuda/cuda116_py39.sif"

TBL_REFIN_SRC="u"
TBL_MESH_SRC="CS"
#TBL_RESL_SRC="0 1 2 3 4"
TBL_RESL_SRC="2"

TBL_REFIN_TGT="u"
TBL_MESH_TGT="ICOD"
#TBL_RESL_TGT="0 1 2 3 4"
TBL_RESL_TGT="2"

ITERMAX=100

OVERWRITE_RT=true
OVERWRITE_SUMMARY=true

echo "============ STEP ${STEP} ============"
for REFIN_SRC in ${TBL_REFIN_SRC}; do
for MESH_SRC in ${TBL_MESH_SRC}; do
for RESL_SRC in ${TBL_RESL_SRC}; do
for REFIN_TGT in ${TBL_REFIN_TGT}; do
for MESH_TGT in ${TBL_MESH_TGT}; do
for RESL_TGT in ${TBL_RESL_TGT}; do

  OPT_OVERWRITE=""
  ${OVERWRITE_RT} && OPT_OVERWRITE="${OPT_OVERWRITE} --overwrite_rt"
  ${OVERWRITE_SUMMARY} && OPT_OVERWRITE="${OPT_OVERWRITE} --overwrite_summary"

  OPT_SRC_MESH="${REFIN_SRC} ${MESH_SRC} ${RESL_SRC}"
  OPT_TGT_MESH="${REFIN_TGT} ${MESH_TGT} ${RESL_TGT}"
  OPT_OTHERS="${OPT_OVERWRITE}"

  [ "${OPT_SRC_MESH}" == "${OPT_TGT_MESH}" ] && continue

  JOB_ID="${REFIN_SRC}${MESH_SRC}${RESL_SRC}_${REFIN_TGT}${MESH_TGT}${RESL_TGT}"
  WDATE=`date +"%Y%m%d%H%M%S"`

  DIR_LOG_THIS="${DIR_LOG}/child/${NAME_SELF}"
  FILE_LOG="${DIR_LOG_THIS}/${JOB_ID}_${WDATE}.txt"
  mkdir -p ${DIR_LOG_THIS}

  DIR_SCRIPT_THIS="${DIR_SCRIPT}/child/${NAME_SELF}"
  FILE_SCRIPT="${DIR_SCRIPT_THIS}/${JOB_ID}.sh"
  mkdir -p ${DIR_SCRIPT_THIS}

  cat << EOF > ${FILE_SCRIPT}
#!/bin/bash
set -e
set -x

source ${FILE_PYENV}

date

python ${FILE_PYTHON_SCRIPT} measure_time ${OPT_SRC_MESH} ${OPT_TGT_MESH} ${ITERMAX} ${OPT_OTHERS}

date
EOF

  chmod 744 ${FILE_SCRIPT}

  srun --chdir `pwd` -o ${FILE_LOG} -e ${FILE_LOG} singularity exec ${FILE_SIF} ./${FILE_SCRIPT} &

  echo "${WDATE} ${FILE_SCRIPT}"
  echo "${WDATE} ${FILE_SCRIPT}" >> ${FILE_SUBMISSION_HISTORY}

  #sleep 1

done  # RESL_TGT
done  # MESH_TGT
done  # REFIN_TGT
done  # RESL_SRC
done  # MESH_SRC
done  # REFIN_SRC
