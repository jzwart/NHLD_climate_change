# analysis of condor output; JAZ 2018-01-10 

# aggregating future scenario runs 
agg_future_scenarios <- function(ind_file, gd_config){
  
  list_out <- list()
  
  dir <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/Condor_Results/'
  dir_lookup <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/'

  scenarios <- list.files(dir)
  scenario_lookup <- read.csv(file.path(dir_lookup,'scenarios.csv'),
                              stringsAsFactors = F) # 
  
  skip=6*365 # days to skip; first 6 years (spin up)
  
  watersheds<-read.table('1_data/in/NHLDsheds_20170323.txt',
                         stringsAsFactors = F,
                         header=T,
                         sep = '\t')
  
  threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes 
  for(i in 1:length(scenarios)){
    print(i)

    cur_scenario_id <- strsplit(scenarios[i],'_')[[1]][2]
    cur_scenario <- scenario_lookup$scenario[scenario_lookup$job==cur_scenario_id]
    
    condor_lookup <- read.csv(file.path(dir_lookup,paste('allruns_',cur_scenario_id,'.csv',sep='')),stringsAsFactors = F)
    
    sub_dir <- file.path(dir,scenarios[i])
    
    files <- list.files(sub_dir)
    if(length(files[grep('adj',files)])>0){
      files <- files[-grep('adj',files)]
    }
    
    sum <- data.frame() # open ice only
    all <- data.frame() 
    for(j in 1:length(files)){ # length(files
      print(c(i,j))
      
      cur <- readRDS(file.path(sub_dir,files[j]))
      lake<-strsplit(tolower(files[j]),'_c_model.rds',fixed = T)[[1]]
      if(scenarios[i]!='Present'){ # all scenarios other than Present were run on Condor and need Condor lookup table 
        lake <- condor_lookup$currID[condor_lookup$crun==as.numeric(lake)]
      }
      cur<-na.omit(cur)
      if(length(cur$time)<2){
        next
      }
      lake <- toupper(lake) # making all uppercase; important for JAZ and ZJH lakes 
      
      cur<-cur[skip:(nrow(cur)-0),3:ncol(cur)] # skipping first X number of days
      cur<-cur[cur$Vol>0,] # only keeping days when there's actually water
      curSum=cur[cur$LakeE>0,] # only open ice periods
      ndays_open_ice <- nrow(curSum)
      ndays_ice <- nrow(cur[cur$LakeE==0,])
      
      # removing outliers based on % of original volume 
      vol_frac <- cur$Vol/cur$Vol[1]
      cur <- cur[vol_frac>threshold,]
      
      cur<-data.frame(t(apply(cur,MARGIN = 2,FUN = mean)))
      curSum<-data.frame(t(apply(curSum,MARGIN = 2,FUN = mean)))
      cur$Permanent_<-lake
      curSum$Permanent_<-lake
      curSum$ndays_open_ice <- ndays_open_ice
      curSum$ndays_ice <- ndays_ice
      cur$ndays_open_ice <- ndays_open_ice
      cur$ndays_ice <- ndays_ice
      
      sum<-rbind(sum,curSum)
      all<-rbind(all,cur)
    }
    q = i + (i-1)
    
    sum <- merge(sum, watersheds, by='Permanent_')
    all <- merge(all, watersheds, by='Permanent_')
    
    out_name_all = paste(cur_scenario, 'all', sep = '_')
    out_name_sum = paste(cur_scenario, 'sum', sep = '_') 
    
    list_out[[q]] <- all
    list_out[[q+1]] <- sum
    
    names(list_out)[q:(q+1)] <- c(out_name_all, out_name_sum)
  }
  
  data_file = as_data_file(ind_file)
  saveRDS(list_out, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

# aggregating the retro model runs 

agg_retro <- function(ind_file, gd_config){
  # for present day results 
  
  list_out <- list()
  
  dir <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/Present/20170819/'
  
  skip=6*365 # days to skip; first 6 years (spin up)
  
  watersheds<-read.table('1_data/in/NHLDsheds_20170323.txt',
                         stringsAsFactors = F,
                         header=T,
                         sep = '\t')
  
  threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes 
  
  cur_scenario <- 'Present'
  
  files <- list.files(dir)
  if(length(files[grep('adj',files)])>0){
    files <- files[-grep('adj',files)]
  }
  
  sum <- data.frame() # open ice only
  all <- data.frame() 
  for(j in 1:length(files)){ # length(files
    print(c(i,j))
    
    cur <- readRDS(file.path(dir,files[j]))
    lake<-strsplit(tolower(files[j]),'_c_model.rds',fixed = T)[[1]]
    
    cur<-na.omit(cur)
    if(length(cur$time)<2){
      next
    }
    lake <- toupper(lake) # making all uppercase; important for JAZ and ZJH lakes 
    
    cur<-cur[skip:(nrow(cur)-0),3:ncol(cur)] # skipping first X number of days
    cur<-cur[cur$Vol>0,] # only keeping days when there's actually water
    curSum=cur[cur$LakeE>0,] # only open ice periods
    ndays_open_ice <- nrow(curSum)
    ndays_ice <- nrow(cur[cur$LakeE==0,])
    
    # removing outliers based on % of original volume 
    vol_frac <- cur$Vol/cur$Vol[1]
    cur <- cur[vol_frac>threshold,]
    
    cur<-data.frame(t(apply(cur,MARGIN = 2,FUN = mean)))
    curSum<-data.frame(t(apply(curSum,MARGIN = 2,FUN = mean)))
    cur$Permanent_<-lake
    curSum$Permanent_<-lake
    curSum$ndays_open_ice <- ndays_open_ice
    curSum$ndays_ice <- ndays_ice
    cur$ndays_open_ice <- ndays_open_ice
    cur$ndays_ice <- ndays_ice
    
    sum<-rbind(sum,curSum)
    all<-rbind(all,cur)
  }
  sum <- merge(sum, watersheds, by='Permanent_')
  all <- merge(all, watersheds, by='Permanent_')
  
  out_name_all = paste(cur_scenario, 'all', sep = '_')
  out_name_sum = paste(cur_scenario, 'sum', sep = '_') 
  
  list_out[[1]] <- all
  list_out[[2]] <- sum
  
  names(list_out)[1:2] <- c(out_name_all, out_name_sum)
  
  data_file = as_data_file(ind_file)
  saveRDS(list_out, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
