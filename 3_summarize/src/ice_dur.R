ice_dur <- function(ind_file, var_lookup_yml, vars_yml, remake_file, gd_config){

  dir <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/Condor_Results/'
  dir_lookup <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/'

  scenarios <- list.files(dir)
  scenario_lookup <- read.csv(file.path(dir_lookup,'scenarios.csv'),
                              stringsAsFactors = F) #

  # watersheds<-read.table('1_data/in/NHLDsheds_20170323.txt',
  #                        stringsAsFactors = F,
  #                        header=T,
  #                        sep = '\t')

  skip=6*365 # days to skip; first 6 years (spin up)

  threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes

  all <- data_frame()
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

    cur_gcm = strsplit(cur_scenario, '_2')[[1]][1]
    cur_period = substr(cur_scenario, nchar(cur_scenario)-4, nchar(cur_scenario))

    cur <- readRDS(file.path(sub_dir,files[1])) %>%
      slice(skip:nrow(.)) %>% # skipping first X number of days
      dplyr::filter(Vol > 0) %>% # only keeping days when there's actually water
      mutate(vol_frac = Vol / Vol[1]) %>%  # removing outliers based on % of original volume
      dplyr::filter(vol_frac > threshold) %>%
      select(datetime, LakeE) %>%
      mutate(year = strftime(datetime, format = '%Y', tz ='GMT'),
             ice_on = as.numeric(LakeE == 0),
             month = strftime(datetime, '%m', tz ='GMT'),
             doy = LakeMetabolizer:::date2doy(as.POSIXct(datetime)),
             gcm = cur_gcm,
             period = cur_period) # grouping into years for lake ice duration plot

    out <- cur

    all <- rbind(all, out)
  }

  # for retro results
  dir <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/Present/20170819/'

  cur_scenario <- 'Present'

  cur_gcm = 'Retro'
  cur_period = 'Retro'

  files <- list.files(dir)
  if(length(files[grep('adj',files)])>0){
    files <- files[-grep('adj',files)]
  }

  cur <- readRDS(file.path(dir,files[1])) %>%
    slice(skip:nrow(.)) %>% # skipping first X number of days
    dplyr::filter(Vol > 0) %>% # only keeping days when there's actually water
    mutate(vol_frac = Vol / Vol[1]) %>%  # removing outliers based on % of original volume
    dplyr::filter(vol_frac > threshold) %>%
    select(datetime, LakeE) %>%
    mutate(year = strftime(datetime, format = '%Y', tz ='GMT'),
           ice_on = as.numeric(LakeE == 0),
           month = strftime(datetime, '%m', tz ='GMT'),
           doy = LakeMetabolizer:::date2doy(as.POSIXct(datetime)),
           gcm = cur_gcm,
           period = cur_period) # grouping into years for lake ice duration plot

  out <- cur

  all <- rbind(all, out)

  data_file = as_data_file(ind_file)
  saveRDS(all, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
