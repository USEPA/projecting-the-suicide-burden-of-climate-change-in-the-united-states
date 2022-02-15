#!/bin/bash

CONFIG="configuration.yaml"
DISCOUNT_RATE=3
DISCOUNT_YEAR=2015


POINT_MODE="TRUE"

for DEGREE in "D1" "D2" "D3" "D4" "D5" "D6" ; do
  for POPULATION_YEAR in "PRESENT" "FUTURE" ; do
    for INCOME_YEAR in "PRESENT" "FUTURE" ; do
      if [[ "${POPULATION_YEAR}" = "PRESENT" && "${INCOME_YEAR}" = "FUTURE" ]]; then
       continue
      fi
      Rscript simulate/estimate.R  ${CONFIG} ${DEGREE} ${POINT_MODE} ${POPULATION_YEAR} ${INCOME_YEAR} ${DISCOUNT_RATE} ${DISCOUNT_YEAR}
    done
  done
done

POINT_MODE="FALSE"

for DEGREE in "D1" "D2" "D3" "D4" "D5" "D6" ; do
  for POPULATION_YEAR in "PRESENT" "FUTURE" ; do
    for INCOME_YEAR in "PRESENT" "FUTURE" ; do
      if [[ "${POPULATION_YEAR}" = "PRESENT" && "${INCOME_YEAR}" = "FUTURE" ]]; then
       continue
      fi
      Rscript simulate/estimate.R  ${CONFIG} ${DEGREE} ${POINT_MODE} ${POPULATION_YEAR} ${INCOME_YEAR} ${DISCOUNT_RATE} ${DISCOUNT_YEAR}
      Rscript simulate/testConvergence.R  ${CONFIG} ${DEGREE} ${POINT_MODE} ${POPULATION_YEAR} ${INCOME_YEAR} ${DISCOUNT_RATE} ${DISCOUNT_YEAR}
    done
  done
done



