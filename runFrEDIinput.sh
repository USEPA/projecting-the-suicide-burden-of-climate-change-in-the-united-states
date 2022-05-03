#!/bin/bash

CONFIG="configuration.yaml"


for DEGREE in "D1" "D2" "D3" "D4" "D5" "D6" ; do
  for POPULATION_YEAR in "PRESENT" "FUTURE" ; do
    Rscript simulate/genInput4FrEDI.R  ${CONFIG} ${DEGREE} ${POPULATION_YEAR} 
  done
done




