
conc_sens_compare <- function(ind_file, n_draws, gd_config){

  conc_sens <- readRDS('4_conc_sens/out/conc_sens_summary.rds') %>% as_tibble() %>%
    mutate(scenario = paste(doc_change, dic_change, p_change, sep ='_'))

  scenarios <- readRDS('1_data/out/scenarios_qa.rds') %>%
    dplyr::filter(period == '2080s', gcm == 'CESM1_CAM5', season =='all') %>%
    mutate(doc_change = 1,
           dic_change = 1,
           p_change = 1,
           scenario = paste(doc_change, dic_change, p_change, sep ='_')) %>%
    select(colnames(conc_sens))

  conc_sens <- conc_sens %>%  # only keeping lakes that are in scenario file
    group_by(scenario) %>%
    dplyr::filter(Permanent_ %in% scenarios$Permanent_) %>%
    ungroup()

  all <- bind_rows(conc_sens, scenarios) # all 9 scenarios (8 +/- and 1 with no change)

  total <- all %>%
    group_by(scenario) %>%
    summarise(Emit = sum(Emit * 12 * 365)) %>%
    ungroup()

  # would we expect a uniform increase / decrease in concentration change across all the NHLD? probably not...
  #  we could sample each lake from the 9 scenarios, re-calculate total emissions, and do this 10,000 times to create a distribution;
  #  should be OK since all lakes are run independently of each other

  n_draws = n_draws # number of times to draw
  lakes = unique(all$Permanent_)

  total_mcmc <- data.frame()
  for(i in 1:n_draws){

    cur_lake_sample = data.frame()
    for(j in 1:length(lakes)){
      print(c(i,j))
      cur = dplyr::filter(all, Permanent_ == lakes[j]) %>% dplyr::slice(sample(1:9, size = 1)) # randomly selecting row for each lake
      cur_lake_sample <- bind_rows(cur_lake_sample, cur)
    }

    cur_total <- cur_lake_sample %>%
      summarise(Emit = sum(Emit * 12 * 365),
                Bury = sum(Burial_total * 12 *365))

    total_mcmc <- bind_rows(total_mcmc, cur_total)
  }


  # total_mcmc

  data_file <- as_data_file(ind_file)
  saveRDS(total_mcmc, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

