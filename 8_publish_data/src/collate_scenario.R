# collating scenarios for publishing on ScienceBase; JAZ 2019-05-29

collate_scenario <- function(data_file,
                             scenarios_lookup_dir,
                             results_dir,
                             gd_config){

  scenarios_lookup <- read.csv(file.path(scenarios_lookup_dir, 'scenarios.csv'), stringsAsFactors = F)
  scenario <- gsub(pattern = '.rds', '', gsub(pattern = '8_publish_data/out/',replacement = '', data_file))
  condor_job <- scenarios_lookup$job[scenarios_lookup$scenario == scenario]

  skip=6*365 # days to skip; first 6 years (spin up)

  watersheds<-read.table('1_data/in/NHLDsheds_20170323.txt',
                         stringsAsFactors = F,
                         header=T,
                         sep = '\t')

  threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes

  condor_lookup <- read.csv(file.path(scenarios_lookup_dir, paste('allruns_', condor_job,'.csv', sep='')), stringsAsFactors = F)

  sub_dir <- file.path(results_dir, paste0('results_', condor_job))

  files <- list.files(sub_dir)
  if(length(files[grep('adj',files)])>0){
    files <- files[-grep('adj',files)]
  }

  out = lapply(files, function(cur_file){
    print(cur_file)

    cur <- readRDS(file.path(sub_dir, cur_file))
    lake <- strsplit(tolower(cur_file),'_c_model.rds',fixed = T)[[1]]
    cur_force <- condor_lookup$force[condor_lookup$crun == as.numeric(lake)]

    lake <- condor_lookup$currID[condor_lookup$crun == as.numeric(lake)]

    cur <- na.omit(cur)
    if(length(cur$time)<2){
      next
    }
    lake <- toupper(lake) # making all uppercase; important for JAZ and ZJH lakes

    cur <- cur[skip:(nrow(cur)-0), ] # skipping first X number of days

    # removing outliers based on % of original volume
    vol_frac <- cur$Vol / cur$Vol[1]
    cur <- cur[vol_frac > threshold, ]

    cols_to_get_rid_of = c('IceSnow', 'LandMelt', 'SWoutMinusLandMelt', 'LandMeltEst', 'Radius')
    cols_not_to_round = c('time', 'retro_datetime_ref', 'Permanent_', 'forcing_file', 'emergent_d_epi', 'emergent_d_hypo', 'GPP')

    cur <- cur %>%
      mutate(Permanent_ = lake,
             forcing_file = cur_force) %>%
      rename(retro_datetime_ref = datetime) %>%
      select(-cols_to_get_rid_of) %>%
      mutate_at(colnames(.)[!colnames(.) %in% cols_not_to_round], round, 3)

  }) %>% bind_rows()

  saveRDS(out, data_file)
}

# collating the retro model runs

collate_retro <- function(ind_file, gd_config){
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
