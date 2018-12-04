gather_monthly_drivers <- function(ind_file, gd_config){

  dir <- '1_data/in/Monthly_Avg_WYs_1980_2013/'
  files <- list.files(dir)
  files <- files[grep('.txt', files)]
  files <- files[-grep('Units_formatting.txt', files)]

  out <- data_frame()
  for(i in 1:length(files)){
    # Units in mm, except Temp (deg C) and Snow Depth (cm)
    #
    # rows, month
    # cols, GCMs
    #
    # GCMs (1:6)
    #
    # CESM1_CAM5,FIO_ESM,GFDL_CM3,GFDL_ESM2M,HadGEM2_AO,HadGEM2_CC
    cur_period = as.character(strsplit(strsplit(files[i], '_')[[1]][stringr::str_count(files[i],'_')+1], '.txt')[[1]][1])
    cur_var = as.character(strsplit(files[i], paste0('_',cur_period, '.txt')))
    if(cur_period == 'Hist'){cur_period = 'Retro'}

    if(cur_period == 'Retro'){
      cur <- read.table(file.path(dir,files[i]), stringsAsFactors = F, sep =',') %>% as_tibble() %>%
        magrittr::set_colnames('var_value') %>%
        mutate(month = 1:12,
               period = cur_period,
               var = !!cur_var,
               gcm = 'Retro') %>%
        select(period, gcm, month, var, var_value)
    }else{
      cur <- read.table(file.path(dir,files[i]), stringsAsFactors = F, sep =',') %>% as_tibble() %>%
        magrittr::set_colnames(c('CESM1_CAM5','FIO_ESM','GFDL_CM3','GFDL_ESM2M','HadGEM2_AO','HadGEM2_CC')) %>%
        mutate(month = 1:12,
               period = cur_period) %>%
        gather(key = 'gcm', value = 'var_value', contains('_')) %>%
        mutate(var = !!cur_var) %>%
        select(period, gcm, month, var, var_value)
    }

    out <- bind_rows(out, cur)
  }

  # baseflow + runoff
  r <- out %>% dplyr::filter(var =='Runoff')
  b <- out %>% dplyr::filter(var == 'Baseflow')
  r_b <- left_join(r, b, by = c('period' = 'period', 'gcm'='gcm', 'month'='month')) %>%
    mutate(var = 'Runoff_and_baseflow',
           var_value = var_value.x + var_value.y) %>%
    select(period, gcm, month, var, var_value)

  p <- out %>% dplyr::filter(var =='Precip')
  e <- out %>% dplyr::filter(var == 'Evap')
  p_e <- left_join(p, e, by = c('period' = 'period', 'gcm'='gcm', 'month'='month')) %>%
    mutate(var = 'P-E',
           var_value = var_value.x - var_value.y) %>%
    select(period, gcm, month, var, var_value)

  out <- bind_rows(out, r_b, p_e)

  data_file = as_data_file(ind_file)
  saveRDS(out, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
