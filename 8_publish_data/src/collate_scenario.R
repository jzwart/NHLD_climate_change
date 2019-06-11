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

collate_retro <- function(data_file,
                          forcing_file,
                          results_dir,
                          gd_config){

  # for retrospecitive model runs

  skip=6*365 # days to skip; first 6 years (spin up)

  threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes

  forcing_lookup = read.csv(forcing_file, stringsAsFactors = F)

  cur_scenario <- 'retro'

  files <- list.files(results_dir)
  if(length(files[grep('adj',files)])>0){
    files <- files[-grep('adj',files)]
  }

  out = lapply(files, function(cur_file){
    print(cur_file)

    cur <- readRDS(file.path(results_dir, cur_file))
    lake <- strsplit(tolower(cur_file),'_c_model.rds',fixed = T)[[1]]

    cur <- na.omit(cur)
    if(length(cur$time)<2){
      next
    }
    lake <- toupper(lake) # making all uppercase; important for JAZ and ZJH lakes

    cur <- cur[skip:(nrow(cur)-0), ] # skipping first X number of days

    # removing outliers based on % of original volume
    vol_frac <- cur$Vol / cur$Vol[1]
    cur <- cur[vol_frac > threshold, ]

    cur_force = forcing_lookup$force[forcing_lookup$currID == lake]

    cols_to_get_rid_of = c('IceSnow', 'LandMelt', 'SWoutMinusLandMelt', 'LandMeltEst', 'Radius')
    cols_not_to_round = c('time', 'datetime', 'Permanent_', 'forcing_file', 'emergent_d_epi', 'emergent_d_hypo', 'GPP')

    cur <- cur %>%
      mutate(Permanent_ = lake,
             forcing_file = cur_force) %>%
      select(-cols_to_get_rid_of) %>%
      mutate_at(colnames(.)[!colnames(.) %in% cols_not_to_round], round, 3)

  }) %>% bind_rows()

  saveRDS(out, data_file)

}
