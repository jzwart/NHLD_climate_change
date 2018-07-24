
library(dplyr)
files <- list.files('4_conc_sens/out/results_6/')
scenario_lookup <- read.csv('D:/MyPapers/NHLD Climate Change/Results/C_model_output/scenarios.csv',stringsAsFactors = F)

# # scenario time period
period <- '2080s' # picking most extreme hydrology scenarios to cross with concentration scenarios
scenario <- 'CESM1_CAM5' # most middle of the road GCM - should be representative of median
scenario_id = scenario_lookup$job[scenario_lookup$scenario==paste(scenario, period, sep='_')]
allruns<-read.csv(paste('../rlake/data/allruns_',scenario_id,'.csv',sep = ''), stringsAsFactors=FALSE) # lookup table for perm_id

out <- dplyr::data_frame()
for(i in 1:length(files)){
  lake <- strsplit(files[i], split = '_')[[1]][1]
  cur <- readRDS(file.path('4_conc_sens/out/results_6/',files[i])) %>%
    summarise_all(funs(mean) , na.rm =T) %>%
    dplyr::mutate(run = files[i], lake = lake, Permanent_ = allruns$currID[allruns$crun == lake])
  out <- rbind(out, cur)
}
lakes <- unique(out$lake)

dir <- paste0('D:/MyPapers/NHLD Climate Change/Results/C_model_output/Condor_Results/results_', scenario_id)
for(i in 1:length(lakes)){
  cur_lake <- allruns$currID[allruns$crun == lakes[i]]
  cur <- readRDS(file.path(dir, paste0(lakes[i],'_C_model.Rds'))) %>%
    mutate(datetime = as.Date(datetime)) %>%
    summarise_all(funs(mean) , na.rm =T) %>%
    dplyr::mutate(run = 'no_conc_change', lake = lakes[i], Permanent_ = cur_lake)
  out <- rbind(out, cur)
}

no_change <- out %>%
  dplyr::filter(run == 'no_conc_change') %>%
  group_by(Permanent_) %>%
  summarise_all(funs(max)) %>%
  ungroup() %>%
  tidyr::gather('var', 'value_no_change', 2:57)

range <- out %>%
  group_by(Permanent_) %>%
  summarise_all(funs(max, min)) %>%
  ungroup() %>%
  tidyr::gather('var_max', 'value_max', contains('max')) %>%
  tidyr::gather('var_min', 'value_min', contains('min')) %>%
  mutate(diff = as.numeric(value_max) - as.numeric(value_min),
         ratio = as.numeric(value_max) / as.numeric(value_min),
         var = lapply(var_max, function(var){strsplit(var, '_max')[[1]][1] %>% as_data_frame()}) %>%
           bind_rows() %>%
           pull(value),
         var1 = lapply(var_min, function(var){strsplit(var, '_min')[[1]][1] %>% as_data_frame()}) %>%
           bind_rows() %>%
           pull(value)) %>%
  dplyr::filter(var == var1) %>%
  select(-var_max, -var_min, -var1) %>%
  left_join(no_change, by = c('Permanent_', 'var'))

diff <- range %>%
  mutate(max_no_ratio = as.numeric(value_max) / as.numeric(value_no_change),
         min_no_ratio = as.numeric(value_min) / as.numeric(value_no_change)) %>%
  tidyr::gather('ratio', 'value', contains('no_ratio')) %>%
  dplyr::filter(!var %in% c('Area', 'Baseflow', 'co2', 'datetime', 'DirectP', 'SWin', 'Elev', 'GWin', 'GWout', 'SWout', 'hypoTemp', 'IceMelt',
                            'IceSnow', 'lake', 'LakeE', 'LandMelt', 'Perim', 'run', 'Radius', 'snowDepth_m', 'Stage',
                            'SWoutMinusLandMelt', 'time', 'Vol', 'LandMeltEst'))

library(ggplot2)

windows()

ggplot(diff, aes(x = value, group = ratio, fill = ratio)) +
  geom_histogram(bins = 10) +
  # geom_freqpoly(bins = 10) +
  facet_wrap(~var, scales = 'free') +
  theme_classic() +
  labs(y = 'Frequency', x = 'Ratio') +
  scale_fill_manual(name = 'ratio',
                    values = c('red', 'blue'),
    breaks=c("max_no_ratio", "min_no_ratio"),
                    labels=c("Max Response", "Min Response"))



