

rm(list = ls())

dir <- 'F:/Jake/My Papers/NHLD Climate Change/Results/C_model_output/Condor_Results/results_11/'

library(plyr)

zipF <- list.files(path = dir, pattern = "*.zip",full.names = T)

setwd(dir)
ldply(.data = zipF, .fun = unzip)


files <- list.files(dir)

rds = unlist(strsplit(files[grep('.Rds',files)],split = '_C_model.Rds',fixed = T))
zip = unlist(strsplit(files[grep('.zip',files)],split = '_C_model.zip',fixed = T))

torm = rds[rds%in%zip]

torm = paste(torm,'_C_model.zip',sep='')
torm = paste(dir,torm,sep='')

do.call(file.remove,list(torm))



