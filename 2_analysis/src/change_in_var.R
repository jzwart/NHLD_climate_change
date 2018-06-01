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
  }else if(var_cfg$variable == 'pco2'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             grep(var_cfg$variable, tolower(colnames(.)))) %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_pco2 = mean(pco2)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_pco2 = mean_pco2)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_pco2 = mean_pco2 - retro_pco2,
             ratio_pco2 = mean_pco2 / retro_pco2)
  }else if(var_cfg$variable == 'emit_areal'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'Emit', 'Area') %>%
      mutate(emit_areal = Emit / Area * 12 * 365) %>% # areal emissions in g / m2 / year
      group_by(Permanent_, period, season) %>%
      summarise(mean_emit_areal = mean(emit_areal)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_emit_areal = mean_emit_areal)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_emit_areal = mean_emit_areal - retro_emit_areal,
             ratio_emit_areal = mean_emit_areal / retro_emit_areal)
  }else if(var_cfg$variable == 'emit'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'Emit') %>%
      mutate(emit = Emit * 12 * 365) %>% # areal emissions in g / year
      group_by(Permanent_, period, season) %>%
      summarise(mean_emit = mean(emit)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_emit = mean_emit)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_emit = mean_emit - retro_emit,
             ratio_emit = mean_emit / retro_emit)
  }else if(var_cfg$variable == 'area'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'Area') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_area = mean(Area)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_area = mean_area)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_area = mean_area - retro_area,
             ratio_area = mean_area / retro_area)
  }else if(var_cfg$variable == 'fhee'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'percentEvap') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_fhee = mean(percentEvap)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_fhee = mean_fhee)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_fhee = mean_fhee - retro_fhee,
             ratio_fhee = mean_fhee / retro_fhee)
  }else if(var_cfg$variable == 'bury'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'Burial_total') %>%
      mutate(bury = Burial_total * 12 * 365) %>% # burial in g C / year
      group_by(Permanent_, period, season) %>%
      summarise(mean_bury = mean(bury)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_bury = mean_bury)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_bury = mean_bury - retro_bury,
             ratio_bury = mean_bury / retro_bury)
  }else if(var_cfg$variable == 'bury_areal'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'Burial_total', 'Area') %>%
      mutate(bury_areal = Burial_total / Area * 12 * 365) %>% # burial in g C / m2/  year
      group_by(Permanent_, period, season) %>%
      summarise(mean_bury_areal = mean(bury_areal)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_bury_areal = mean_bury_areal)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_bury_areal = mean_bury_areal - retro_bury_areal,
             ratio_bury_areal = mean_bury_areal / retro_bury_areal)
  }else if(var_cfg$variable == 'sw_in'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'SWin', 'Baseflow') %>%
      mutate(SWin = SWin + Baseflow) %>% # adding baseflow and runoff together
      group_by(Permanent_, period, season) %>%
      summarise(mean_sw_in = mean(SWin)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_sw_in = mean_sw_in)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_sw_in = mean_sw_in - retro_sw_in,
             ratio_sw_in = mean_sw_in / retro_sw_in)
  }else if(var_cfg$variable == 'gw_in'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'GWin') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_gw_in = mean(GWin)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_gw_in = mean_gw_in)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_gw_in = mean_gw_in - retro_gw_in,
             ratio_gw_in = mean_gw_in / retro_gw_in)
  }else if(var_cfg$variable == 'gw_out'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'GWout') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_gw_out = mean(GWout)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_gw_out = mean_gw_out)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_gw_out = mean_gw_out - retro_gw_out,
             ratio_gw_out = mean_gw_out / retro_gw_out)
  }else if(var_cfg$variable == 'sw_out'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'SWout') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_sw_out = mean(SWout)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_sw_out = mean_sw_out)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_sw_out = mean_sw_out - retro_sw_out,
             ratio_sw_out = mean_sw_out / retro_sw_out)
  }else if(var_cfg$variable == 'precip'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'DirectP', 'Area') %>%
      mutate(DirectP = DirectP / Area) %>% # precip in m
      group_by(Permanent_, period, season) %>%
      summarise(mean_precip_m = mean(DirectP)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_precip_m = mean_precip_m)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_precip_m = mean_precip_m - retro_precip_m,
             ratio_precip_m = mean_precip_m / retro_precip_m)
  }else if(var_cfg$variable == 'evap'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'LakeE', 'Area') %>%
      mutate(LakeE = LakeE / Area) %>% # evap in m
      group_by(Permanent_, period, season) %>%
      summarise(mean_evap_m = mean(LakeE)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_evap_m = mean_evap_m)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_evap_m = mean_evap_m - retro_evap_m,
             ratio_evap_m = mean_evap_m / retro_evap_m)
  }else if(var_cfg$variable == 'hrt'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'HRT') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_hrt = mean(HRT)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_hrt = mean_hrt)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_hrt = mean_hrt - retro_hrt,
             ratio_hrt = mean_hrt / retro_hrt)
  }

  data_file = as_data_file(ind_file)
  saveRDS(scenarios, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
