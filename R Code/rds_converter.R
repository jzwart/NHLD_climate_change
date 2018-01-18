# rds converter 

dir <- '/Users/jzwart/NHLD_climate_change/Results/'

files <- list.files(dir)

for(i in 1:length(files)){
  cur <- strsplit(files[i],'_',fixed = T)[[1]][1]
  rio::convert(file.path(dir,files[i]),out_file = file.path(dir,paste(cur,'_C_model.rds',sep='')))
}
beepr::beep(4)

