#!/bin/csh
#$ -M jzwart@nd.edu
#$ -m abe
#$ -pe mpi-24 48
#$ -q debug
#$ -N NHLD_JAZ_20170620
#$ -r y

module load bio/R

setenv R_LIBS ~/NHLDLakeCarbonModel/R_packages

Rscript NHLD_LakeCarbonModel_V13_parallel_discrete_debug.R
