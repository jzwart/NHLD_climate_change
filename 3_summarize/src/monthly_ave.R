monthly_ave <- function(fig_ind, var_lookup_yml, vars_yml, remake_file, gd_config){
  # read in data and make average response of variable by month; error bars are range of gcms

  vars <- yaml::yaml.load_file(vars_yml)$var # indicates which periods, seasons, and variable we want returned

  var_lookup <- yaml::yaml.load_file(var_lookup_yml) # contains fig labels and units

  y <- lapply(vars, function(var){
    noquote(var_lookup$monthly_var[var][[1]])
  }) %>% unlist() %>% noquote()

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

    for(j in 1:length(files)){
      print(c(i,j))

      cur <- readRDS(file.path(sub_dir,files[j])) %>%
        slice(skip:nrow(.)) %>% # skipping first X number of days
        dplyr::filter(Vol > 0) %>% # only keeping days when there's actually water
        mutate(vol_frac = Vol / Vol[1]) %>%  # removing outliers based on % of original volume
        dplyr::filter(vol_frac > threshold) %>%
        select(datetime, eval(y)) %>%
        mutate(month = strftime(datetime, '%m', tz ='GMT')) %>%
        select(-datetime)

      lake<-strsplit(tolower(files[j]),'_c_model.rds',fixed = T)[[1]]
      if(scenarios[i]!='Present'){ # all scenarios other than Present were run on Condor and need Condor lookup table
        lake <- condor_lookup$currID[condor_lookup$crun==as.numeric(lake)]
      }
      cur<-na.omit(cur)
      if(nrow(cur)<2){
        next
      }
      lake <- toupper(lake) # making all uppercase; important for JAZ and ZJH lakes

      # summarizing by month
      cur <- cur %>%
        mutate(Permanent_ = lake,
               gcm = cur_gcm,
               period = cur_period)

      for(var in 1:length(vars)){
        if(var == 1){
          out <- cur %>%
            group_by(Permanent_, gcm, period, month) %>%
            summarise(!!vars[var] := mean(!!sym(y[var]), na.rm = T)) %>% # summarizing by variable and renaming to variable we want to name
            ungroup()
        }else{
          tmp <- cur %>%
            group_by(Permanent_, gcm, period, month) %>%
            summarise(!!vars[var] := mean(!!sym(y[var]), na.rm = T)) %>% # summarizing by variable and renaming to variable we want to name
            ungroup()
          out <- out %>%
            left_join(tmp, by = c('Permanent_','gcm','period','month'))
        }
      }

      all <- rbind(all, out)
    }
  }

  data_file = as_data_file(ind_file)
  saveRDS(all, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
