#!/bin/bash


DIR_SUBMIT="submit"
DIR_LOG="log"
FILE_PYENV=".venv/bin/activate"
FILE_PYTHON_SCRIPT="src/remap.py"
FILE_SUBMISSION_HISTORY="log/submission_history.txt"
FILE_SIF="/data10/imageshare/cuda/cuda116_py39.sif"

TBL_REFIN_SRC="u"
TBL_MESH_SRC="ICOD"
#TBL_RESL_SRC="0 1 2 3 4"
TBL_RESL_SRC="0 1 2 3 4"
TBL_REFIN_TGT="u"
#TBL_MESH_TGT="CS ICOD RLL"
TBL_MESH_TGT="ICOD"
#TBL_RESL_TGT="0 1 2 3 4"
TBL_RESL_TGT="0 1 2 3 4"
TBL_VAR="A1 A2 TPW CFR TPO"
#TBL_VAR="TPW CFR TPO"
#TBL_VAR="A1 A2 CFR"

#STEP=1  # make_rt

STEP=2  # remap_iter, make_netCDF, calc_metrics
DO_REMAP_ITER=true
DO_MAKE_NETCDF=true
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

  JOB_ID="step${STEP}_${REFIN_SRC}${MESH_SRC}${RESL_SRC}_${REFIN_TGT}${MESH_TGT}${RESL_TGT}"
  WDATE=`date +"%Y%m%d%H%M%S"`

  mkdir -p "${DIR_LOG}/step${STEP}"
  FILE_LOG="${DIR_LOG}/step${STEP}/${JOB_ID}_${WDATE}.txt"

  mkdir -p "${DIR_SUBMIT}/step${STEP}"
  FILE_RUN_SCRIPT="${DIR_SUBMIT}/step${STEP}/${JOB_ID}.sh"

  cat << EOF > ${FILE_RUN_SCRIPT}
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

  chmod 744 ${FILE_RUN_SCRIPT}

  srun --chdir `pwd` -o ${FILE_LOG} -e ${FILE_LOG} singularity exec ${FILE_SIF} ./${FILE_RUN_SCRIPT} &

  echo "${WDATE} ${FILE_RUN_SCRIPT}"
  echo "${WDATE} ${FILE_RUN_SCRIPT}" >> ${FILE_SUBMISSION_HISTORY}

  sleep 1

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

  JOB_ID="step${STEP}_${REFIN_SRC}${MESH_SRC}${RESL_SRC}_${REFIN_TGT}${MESH_TGT}${RESL_TGT}_${VAR}"
  WDATE=`date +"%Y%m%d%H%M%S"`

  mkdir -p "${DIR_LOG}/step${STEP}"
  FILE_LOG="${DIR_LOG}/step${STEP}/${JOB_ID}_${WDATE}.txt"

  mkdir -p "${DIR_SUBMIT}/step${STEP}"
  FILE_RUN_SCRIPT="${DIR_SUBMIT}/step${STEP}/${JOB_ID}.sh"

  OPT_PYTHON_SCRIPT="${OPT_SRC_MESH} ${OPT_TGT_MESH} ${OPT_OTHERS}"

  cat << EOF > ${FILE_RUN_SCRIPT}
#!/bin/bash
set -e
set -x

source ${FILE_PYENV}
EOF

  # Remap iteratively
  if ${DO_REMAP_ITER}; then
    cat << EOF >> ${FILE_RUN_SCRIPT}

date
python ${FILE_PYTHON_SCRIPT} remap_iter ${OPT_PYTHON_SCRIPT}
date
EOF
  fi

  # Make NetCDF files
  if ${DO_MAKE_NETCDF}; then
    cat << EOF >> ${FILE_RUN_SCRIPT}

date
python ${FILE_PYTHON_SCRIPT} make_netCDF ${OPT_PYTHON_SCRIPT}
EOF
  fi

  # Calc. metrics
  if ${DO_CALC_METRICS}; then
    cat << EOF >> ${FILE_RUN_SCRIPT}

date
python ${FILE_PYTHON_SCRIPT} calc_metrics ${OPT_PYTHON_SCRIPT}
EOF
  fi

    cat << EOF >> ${FILE_RUN_SCRIPT}

date
EOF

  chmod 744 ${FILE_RUN_SCRIPT}

  srun --chdir `pwd` -o ${FILE_LOG} -e ${FILE_LOG} singularity exec ${FILE_SIF} ./${FILE_RUN_SCRIPT} &

  echo "${WDATE} ${FILE_RUN_SCRIPT}"
  echo "${WDATE} ${FILE_RUN_SCRIPT}" >> ${FILE_SUBMISSION_HISTORY}

  sleep 1

done  # VAR
done  # RESL_TGT
done  # MESH_TGT
done  # REFIN_TGT
done  # RESL_SRC
done  # MESH_SRC
done  # REFIN_SRC
fi
