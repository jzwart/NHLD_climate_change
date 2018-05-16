# identifying runs that failed on condor and rerunning missing runs 


###########################
load('C:/Users/jzwart/NHLD_climate_change/R Data/c_model_summary.RData')
############

lakes = Present_all$Permanent_

dir <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/Condor_Results/'
dir_lookup <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/'

scenarios <- list.files(dir)
scenario_lookup <- read.csv('D:/MyPapers/NHLD Climate Change/Results/C_model_output/scenarios.csv',stringsAsFactors = F)

out=data.frame()
for(i in 1:nrow(scenario_lookup)){
  cur_scenario <- scenario_lookup$scenario[i]
  
  cur <- eval(parse(text=paste(scenario_lookup$scenario[i],'all',sep = '_')))
  missing_lakes <- lakes[!lakes%in%cur$Permanent_]
  
  if(nrow(cur)==length(lakes)){
    cur_out = data.frame(scenario = cur_scenario, lakes_to_run = NA)
  }else{
    cur_out = data.frame(scenario = rep(cur_scenario, length(missing_lakes)), lakes_to_run = missing_lakes)
  }
  out = rbind(out, cur_out) 
}

out = na.omit(out)

saveRDS(out, '/Users/jzwart/NHLD_climate_change/R Data/missing_lakes_condor.rds')

