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
  }else if(var_cfg$variable == 'gpp'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'GPP') %>%
      mutate(GPP = GPP * 12) %>% # converting from mol C / m3 to g C / m3
      group_by(Permanent_, period, season) %>%
      summarise(mean_gpp = mean(GPP)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_gpp = mean_gpp)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_gpp = mean_gpp - retro_gpp,
             ratio_gpp = mean_gpp / retro_gpp)
  }else if(var_cfg$variable == 'vol_epi'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'Vepi') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_vol_epi = mean(Vepi)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_vol_epi = mean_vol_epi)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_vol_epi = mean_vol_epi - retro_vol_epi,
             ratio_vol_epi = mean_vol_epi / retro_vol_epi)
  }else if(var_cfg$variable == 'doc_loads'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'DOC_Load', 'waterIn') %>%
      mutate(doc_conc = DOC_Load / waterIn * 12,
             DOC_Load = DOC_Load * 12) %>% # mean DOC concentration in load water in g C / m3 ; doc load in g C / day
      group_by(Permanent_, period, season) %>%
      summarise(mean_doc_load = mean(DOC_Load),
                mean_water_load = mean(waterIn),
                mean_doc_load_conc = mean(doc_conc)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_doc_load = mean_doc_load,
             retro_water_load = mean_water_load,
             retro_doc_load_conc = mean_doc_load_conc)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_doc_load = mean_doc_load - retro_doc_load,
             ratio_doc_load = mean_doc_load / retro_doc_load,
             delta_water_load = mean_water_load - retro_water_load,
             ratio_water_load = mean_water_load / retro_water_load,
             delta_doc_load_conc = mean_doc_load_conc - retro_doc_load_conc,
             ratio_doc_load_conc = mean_doc_load_conc / retro_doc_load_conc)
  }else if(var_cfg$variable == 'tp_loads'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'tp_load', 'waterIn') %>%
      mutate(tp_conc = tp_load / waterIn * 31 * 1000,
             tp_load = tp_load * 31 * 1000) %>% # mean TP concentration in load water in mg P / m3 ; tp load in mg P / day
      group_by(Permanent_, period, season) %>%
      summarise(mean_tp_load = mean(tp_load),
                mean_water_load = mean(waterIn),
                mean_tp_load_conc = mean(tp_conc)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_tp_load = mean_tp_load,
             retro_water_load = mean_water_load,
             retro_tp_load_conc = mean_tp_load_conc)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_tp_load = mean_tp_load - retro_tp_load,
             ratio_tp_load = mean_tp_load / retro_tp_load,
             delta_water_load = mean_water_load - retro_water_load,
             ratio_water_load = mean_water_load / retro_water_load,
             delta_tp_load_conc = mean_tp_load_conc - retro_tp_load_conc,
             ratio_tp_load_conc = mean_tp_load_conc / retro_tp_load_conc)
  }else if(var_cfg$variable == 'zmix'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'zmix') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_zmix = mean(zmix)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_zmix = mean_zmix)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_zmix = mean_zmix - retro_zmix,
             ratio_zmix = mean_zmix / retro_zmix)
  }else if(var_cfg$variable == 'ph'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'pH') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_ph = mean(pH)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_ph = mean_ph)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_ph = mean_ph - retro_ph,
             ratio_ph = mean_ph / retro_ph)
  }else if(var_cfg$variable == 'ice_dur'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'ndays_ice', 'ndays_open_ice') %>%
      mutate(ndays = ndays_ice + ndays_open_ice,
             nyears = ndays / 365,
             ice_dur = ndays_ice / nyears) %>%  # days of ice per year (on average)
      group_by(Permanent_, period, season) %>%
      summarise(mean_ice_dur = mean(ice_dur)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_ice_dur = mean_ice_dur)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_ice_dur = mean_ice_dur - retro_ice_dur,
             ratio_ice_dur = mean_ice_dur / retro_ice_dur)
  }else if(var_cfg$variable == 'vol'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'Vol') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_vol = mean(Vol)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_vol = mean_vol)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_vol = mean_vol - retro_vol,
             ratio_vol = mean_vol / retro_vol)
  }else if(var_cfg$variable == 'water_in'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'waterIn') %>%
      group_by(Permanent_, period, season) %>%
      summarise(mean_water_in = mean(waterIn)) %>% #m3 day-1
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_water_in = mean_water_in)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_water_in = mean_water_in - retro_water_in,
             ratio_water_in = mean_water_in / retro_water_in)
  }else if(var_cfg$variable == 'water_out'){
    out <- all_results %>%
      select('Permanent_', 'period', 'season', 'gcm',
             'fluvialOut', 'LakeE') %>%
      mutate(water_out = fluvialOut + LakeE) %>% # m3 day-1
      group_by(Permanent_, period, season) %>%
      summarise(mean_water_out = mean(water_out)) %>%
      ungroup()

    retro <- out %>%
      dplyr::filter(period == 'Retro') %>%
      select(-period) %>%
      rename(retro_water_out = mean_water_out)

    scenarios <- out %>%
      dplyr::filter(period != 'Retro') %>%
      left_join(y = retro, by = c('Permanent_', 'season')) %>%
      mutate(delta_water_out = mean_water_out - retro_water_out,
             ratio_water_out = mean_water_out / retro_water_out)
  }

  data_file = as_data_file(ind_file)
  saveRDS(scenarios, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
