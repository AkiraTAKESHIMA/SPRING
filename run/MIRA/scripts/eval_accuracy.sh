#!/bin/bash

NAME_SELF="eval_accuracy"

DIR_SCRIPT="scripts"
DIR_LOG="log"
FILE_PYENV=".venv/bin/activate"
FILE_PYTHON_SCRIPT="src/remap.py"
FILE_SUBMISSION_HISTORY="log/history.txt"
FILE_SIF="/data10/imageshare/cuda/cuda116_py39.sif"

TBL_REFIN_SRC="r"
TBL_MESH_SRC="CS"
#TBL_RESL_SRC="0 1 2 3 4"
TBL_RESL_SRC="0 1 2"

TBL_REFIN_TGT="r"
#TBL_MESH_TGT="CS ICOD RLL"
TBL_MESH_TGT="ICOD"
#TBL_RESL_TGT="0 1 2 3 4"
TBL_RESL_TGT="0 1 2"

TBL_VAR="A1 A2 TPW CFR TPO"
#TBL_VAR="TPW CFR TPO"
#TBL_VAR="A1"

STEP=1  # make_rt

#STEP=2  # remap_iter, make_NetCDF, calc_metrics
DO_REMAP_ITER=false
DO_MAKE_NETCDF=false
DO_CALC_METRICS=true

OVERWRITE=true

if [ ${STEP} == 1 ]; then
echo "============ STEP ${STEP} ============"
for REFIN_SRC in ${TBL_REFIN_SRC}; do
for MESH_SRC in ${TBL_MESH_SRC}; do
for RESL_SRC in ${TBL_RESL_SRC}; do
for REFIN_TGT in ${TBL_REFIN_TGT}; do
for MESH_TGT in ${TBL_MESH_TGT}; do
for RESL_TGT in ${TBL_RESL_TGT}; do
  if ${OVERWRITE}; then
    OPT_OVERWRITE="--overwrite"
  else
    OPT_OVERWRITE=""
  fi

  OPT_SRC_MESH="${REFIN_SRC} ${MESH_SRC} ${RESL_SRC}"
  OPT_TGT_MESH="${REFIN_TGT} ${MESH_TGT} ${RESL_TGT}"
  OPT_OTHERS="${OPT_OVERWRITE}"

  [ "${OPT_SRC_MESH}" == "${OPT_TGT_MESH}" ] && continue

  JOB_ID="${REFIN_SRC}${MESH_SRC}${RESL_SRC}_${REFIN_TGT}${MESH_TGT}${RESL_TGT}"
  WDATE=`date +"%Y%m%d%H%M%S"`

  DIR_LOG_THIS="${DIR_LOG}/child/${NAME_SELF}/step${STEP}"
  FILE_LOG="${DIR_LOG_THIS}/${JOB_ID}_${WDATE}.txt"
  mkdir -p ${DIR_LOG_THIS}

  DIR_SCRIPT_THIS="${DIR_SCRIPT}/child/${NAME_SELF}/step${STEP}"
  FILE_SCRIPT="${DIR_SCRIPT_THIS}/${JOB_ID}.sh"
  mkdir -p ${DIR_SCRIPT_THIS}

  cat << EOF > ${FILE_SCRIPT}
#!/bin/bash
set -e
set -x

source ${FILE_PYENV}

date

for IS_FORTH in "True" "False"; do
  python ${FILE_PYTHON_SCRIPT} make_rt ${OPT_SRC_MESH} ${OPT_TGT_MESH} ${OPT_OTHERS} \${IS_FORTH}
done

date
EOF

  chmod 744 ${FILE_SCRIPT}

  #srun --chdir `pwd` -o ${FILE_LOG} -e ${FILE_LOG} singularity exec ${FILE_SIF} ./${FILE_SCRIPT} &

  echo "${WDATE} ${FILE_SCRIPT}"
  echo "${WDATE} ${FILE_SCRIPT}" >> ${FILE_SUBMISSION_HISTORY}

  #sleep 1

done  # RESL_TGT
done  # MESH_TGT
done  # REFIN_TGT
done  # RESL_SRC
done  # MESH_SRC
done  # REFIN_SRC
fi

if [ ${STEP} == 2 ]; then
echo "============ STEP ${STEP} ============"
for REFIN_SRC in ${TBL_REFIN_SRC}; do
for MESH_SRC in ${TBL_MESH_SRC}; do
for RESL_SRC in ${TBL_RESL_SRC}; do
for REFIN_TGT in ${TBL_REFIN_TGT}; do
for MESH_TGT in ${TBL_MESH_TGT}; do
for RESL_TGT in ${TBL_RESL_TGT}; do
for VAR in ${TBL_VAR}; do
  if ${OVERWRITE}; then
    OPT_OVERWRITE="--overwrite"
  else
    OPT_OVERWRITE=""
  fi

  OPT_SRC_MESH="${REFIN_SRC} ${MESH_SRC} ${RESL_SRC}"
  OPT_TGT_MESH="${REFIN_TGT} ${MESH_TGT} ${RESL_TGT}"
  OPT_OTHERS="${VAR} ${OPT_OVERWRITE}"

  [ "${OPT_SRC_MESH}" == "${OPT_TGT_MESH}" ] && continue

  JOB_ID="${REFIN_SRC}${MESH_SRC}${RESL_SRC}_${REFIN_TGT}${MESH_TGT}${RESL_TGT}_${VAR}"
  WDATE=`date +"%Y%m%d%H%M%S"`

  DIR_LOG_THIS="${DIR_LOG}/child/${NAME_SELF}/step${STEP}"
  FILE_LOG="${DIR_LOG_THIS}/${JOB_ID}_${WDATE}.txt"
  mkdir -p ${DIR_LOG_THIS}

  DIR_SCRIPT_THIS="${DIR_SCRIPT}/child/${NAME_SELF}/step${STEP}"
  FILE_SCRIPT="${DIR_SCRIPT_THIS}/${JOB_ID}.sh"
  mkdir -p ${DIR_SCRIPT_THIS}

  OPT_PYTHON_SCRIPT="${OPT_SRC_MESH} ${OPT_TGT_MESH} ${OPT_OTHERS}"

  cat << EOF > ${FILE_SCRIPT}
#!/bin/bash
set -e
set -x

source ${FILE_PYENV}
EOF

  # Remap iteratively
  if ${DO_REMAP_ITER}; then
    cat << EOF >> ${FILE_SCRIPT}

date
python ${FILE_PYTHON_SCRIPT} remap_iter ${OPT_PYTHON_SCRIPT}
date
EOF
  fi

  # Make NetCDF files
  if ${DO_MAKE_NETCDF}; then
    cat << EOF >> ${FILE_SCRIPT}

date
python ${FILE_PYTHON_SCRIPT} make_NetCDF ${OPT_PYTHON_SCRIPT}
EOF
  fi

  # Calc. metrics
  if ${DO_CALC_METRICS}; then
    cat << EOF >> ${FILE_SCRIPT}

date
python ${FILE_PYTHON_SCRIPT} calc_metrics ${OPT_PYTHON_SCRIPT}
EOF
  fi

    cat << EOF >> ${FILE_SCRIPT}

date
EOF

  chmod 744 ${FILE_SCRIPT}

  #srun --chdir `pwd` -o ${FILE_LOG} -e ${FILE_LOG} singularity exec ${FILE_SIF} ./${FILE_SCRIPT} &

  echo "${WDATE} ${FILE_SCRIPT}"
  echo "${WDATE} ${FILE_SCRIPT}" >> ${FILE_SUBMISSION_HISTORY}

  #sleep 1

done  # VAR
done  # RESL_TGT
done  # MESH_TGT
done  # REFIN_TGT
done  # RESL_SRC
done  # MESH_SRC
done  # REFIN_SRC
fi
