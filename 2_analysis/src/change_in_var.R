# general function for pulling out variables of interest and seeing change in response to scenarios

change_in_var <- function(ind_file, raw_ind_file, remake_file, var_cfg_file, gd_config){

  all_results <- readRDS(sc_retrieve(raw_ind_file, remake_file = remake_file))

  var_cfg <- yaml::yaml.load_file(var_cfg_file) # indicates which periods, seasons, and variable we want returned

  if(var_cfg$variable == 'doc'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             grep(var_cfg$variable, tolower(colnames(.))),
             -grep('respired', tolower(colnames(.))),
             -grep('export', tolower(colnames(.))),
             -grep('load', tolower(colnames(.))),
             'Vepi', 'Vhypo', 'Vol') %>%
      mutate(doc_epi = (DOCr_epi + DOCl_epi) / Vepi * 12,
              doc_hypo = (DOCr_hypo + DOCl_hypo) / Vepi * 12,
              doc_all = (DOCr_epi + DOCl_epi + DOCr_hypo + DOCl_hypo) / Vol *12) %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_doc_epi = mean(doc_epi),
                mean_doc_hypo = mean(doc_hypo),
                mean_doc_all = mean(doc_all)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_doc_epi = mean_doc_epi,
             retro_doc_hypo = mean_doc_hypo,
             retro_doc_all = mean_doc_all)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_doc_epi = mean_doc_epi - retro_doc_epi,
             delta_doc_hypo = mean_doc_hypo - retro_doc_hypo,
             delta_doc_all = mean_doc_all - retro_doc_all,
             ratio_doc_epi = mean_doc_epi / retro_doc_epi,
             ratio_doc_hypo = mean_doc_hypo / retro_doc_hypo,
             ratio_doc_all = mean_doc_all / retro_doc_all)
  }else if(var_cfg$variable == 'stage'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             grep(var_cfg$variable, tolower(colnames(.)))) %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_stage = mean(Stage)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_stage = mean_stage)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_stage = mean_stage - retro_stage,
             ratio_stage = mean_stage / retro_stage)
  }

  data_file = as_data_file(ind_file)
  saveRDS(scenarios, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
