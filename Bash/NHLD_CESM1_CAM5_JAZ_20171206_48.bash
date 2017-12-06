#!/bin/csh
#$ -M jzwart@nd.edu
#$ -m abe
#$ -pe mpi-24 48
#$ -q long
#$ -N NHLD_CESM1_CAM5_JAZ_20171206
#$ -r y

module load bio/R

setenv R_LIBS ~/NHLDLakeCarbonModel/R_packages

Rscript NHLD_LakeCarbonModel_V13_parallel_discrete.R
