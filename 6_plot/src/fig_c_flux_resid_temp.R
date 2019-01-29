fig_c_flux_resid_temp <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  drivers <- readRDS(drivers_file) # meteo drivers for period / gcm

  all <- readRDS(scenarios) %>%
    dplyr::filter(season == 'all') %>%
    mutate(Emit = Emit * 12 * 365, # emissions in g C / year
           Bury = Burial_total * 12 * 365, # bury in g C / year
           Emit_areal = Emit / Area, # emissions in g C/ m2/ year
           Bury_areal = Bury / Area, #  bury in g C / m2 / year
           # R_B = (SWin + Baseflow) / Area_m2,
           Precip_lake = DirectP / Area) %>%
    select(Permanent_, period, gcm, Emit, Bury, Emit_areal, Bury_areal, Area, HRT, Stage, Vol,
           doc_conc,FracRet, DOC_Load, GPP, Vepi, DOC_Respired, sed_resp, Vepi,
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp)

  total <- all %>%
    mutate(GPP_vol = GPP,
           GPP = GPP * Vepi, # converting mol C m-3 day-1 to mol C day-1
           DOC = doc_conc * Vepi) %>%
    group_by(period, gcm) %>%
    dplyr::summarise(Emit = sum(Emit),
              Bury = sum(Bury),
              Area = sum(Area),
              Emit_areal = mean(Emit_areal), # mean for all lakes
              Bury_areal = mean(Bury_areal),
              FracRet = sum(DOC_Load * FracRet) / sum(DOC_Load),
              DOC_Load = sum(DOC_Load),
              Precip_lake = mean(Precip_lake),
              Water_in = sum(waterIn),
              # R_B = median(R_B, na.rm = T),
              ndays_ice = mean(ndays_ice),
              epiTemp = median(epiTemp),
              HRT = median(HRT),
              regional_DOC = sum(doc_conc * Vol) / sum(Vol),
              Vol = sum(Vol),
              Vepi = sum(Vepi),
              DOC = sum(DOC)/sum(Vepi),
              total_resp = sum(DOC_Respired + sed_resp),
              NEP = sum(GPP*.15 - DOC_Respired),
              GPP = sum(GPP),
              GPP_vol = GPP/Vepi,
              DOC_resp = sum(DOC_Respired),
              Sed_resp = sum(sed_resp)) %>%
    ungroup()

  retro <- all %>%
    dplyr::filter(gcm == 'Retro')

  merged <- dplyr::filter(all, gcm != 'Retro') %>%
    left_join(retro, by = 'Permanent_', suffix = c('_future', '_retro')) %>%
    group_by(period_future, gcm_future) %>%
    summarise(frac_doc_increase = sum(doc_conc_future>doc_conc_retro)/n()) %>%
    ungroup()

  ave_drivers <- drivers %>%
    group_by(period, gcm, var) %>%
    dplyr::summarise(var_value = sum(var_value)) %>%
    ungroup() %>%
    spread(key = 'var', value = 'var_value') %>% select(-Temp)

  # temperature is average rather than cumulative like the other vars
  ave_temp <- drivers %>%
    group_by(period, gcm, var) %>%
    dplyr::summarise(var_value = mean(var_value)) %>%
    ungroup() %>%
    spread(key = 'var', value = 'var_value') %>% select(period, gcm, Temp)

  ave_drivers <- left_join(ave_drivers, ave_temp, by = c('period', 'gcm'))

  c_and_drivers <- left_join(total, ave_drivers, by = c('period' = 'period', 'gcm' = 'gcm')) %>%
    left_join(merged, by = c('period' = 'period_future', 'gcm' = 'gcm_future')) %>%
    mutate(period = factor(period, levels = c('Retro', '2050s', '2080s')))

  retro_emit = c_and_drivers$Emit[c_and_drivers$gcm =='Retro']
  retro_bury = c_and_drivers$Bury[c_and_drivers$gcm =='Retro']
  retro_emit_minus_bury = retro_emit-retro_bury
  retro_fracRet = c_and_drivers$FracRet[c_and_drivers$gcm == 'Retro']
  retro_doc_load = c_and_drivers$DOC_Load[c_and_drivers$gcm == 'Retro']
  retro_gpp = c_and_drivers$GPP[c_and_drivers$gcm == 'Retro']
  retro_gpp_vol = c_and_drivers$GPP_vol[c_and_drivers$gcm == 'Retro']
  retro_doc_resp = c_and_drivers$DOC_resp[c_and_drivers$gcm == 'Retro']
  retro_total_resp = c_and_drivers$total_resp[c_and_drivers$gcm == 'Retro']
  retro_nep = c_and_drivers$NEP[c_and_drivers$gcm == 'Retro']

  c_and_drivers <- mutate(c_and_drivers,
                          emit_change = case_when(Emit > retro_emit ~ 1,
                                                  TRUE ~ -1),
                          bury_change = case_when(Emit > retro_emit ~ 1,
                                                  TRUE ~ -1),
                          emit_bury_change = case_when(Emit-Bury > retro_emit_minus_bury ~ 1,
                                                       TRUE ~ -1),
                          fracRet_change = case_when(FracRet > retro_fracRet ~ 1,
                                                  TRUE ~ -1),
                          doc_load_change = case_when(DOC_Load > retro_doc_load ~ 1,
                                                                    TRUE ~ -1),
                          gpp_change = case_when(GPP > retro_gpp ~ 1,
                                                 TRUE ~ -1),
                          gpp_vol_change = case_when(GPP_vol > retro_gpp_vol ~ 1,
                                                 TRUE ~ -1),
                          doc_resp_change = case_when(DOC_resp > retro_doc_resp ~ 1,
                                                      TRUE ~ -1),
                          total_resp_change = case_when(total_resp > retro_total_resp ~ 1,
                                                      TRUE ~ -1),
                          nep_change = case_when(NEP > retro_nep ~ 1,
                                                        TRUE ~ -1),
                          gcm_label = case_when(gcm == 'CESM1_CAM5' ~ 3,
                                                gcm == 'FIO_ESM' ~ 4,
                                                gcm == 'GFDL_CM3' ~ 2,
                                                gcm == 'GFDL_ESM2M' ~ 6,
                                                gcm == 'HadGEM2_AO' ~ 1,
                                                gcm == 'HadGEM2_CC' ~ 5))


  c_and_drivers$delta_emit = abs(c_and_drivers$Emit - retro_emit)/retro_emit * 100 * c_and_drivers$emit_change
  c_and_drivers$p_e = c_and_drivers$Precip - c_and_drivers$Evap
  c_and_drivers$emit_resid = resid(lm(c_and_drivers$delta_emit~c_and_drivers$p_e))
  summary(lm(c_and_drivers$emit_resid~c_and_drivers$Temp))

  temp_emit_resid = ggplot(c_and_drivers, aes(x = Temp, y = emit_resid,
                                       color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_point() +
    geom_text(hjust = 0, nudge_x = .2, nudge_y = .4, show.legend = F, size = 6) +
    theme_classic() +
    ylab(expression(Delta~mol~C~emissions~('%')~(P-E)~~residuals)) +
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 15,
                                  '2080s' = 17,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  temp_emit_resid


  c_and_drivers$delta_bury = abs(c_and_drivers$Bury - retro_bury)/retro_bury * 100 * c_and_drivers$bury_change
  c_and_drivers$bury_resid = resid(lm(c_and_drivers$delta_bury~c_and_drivers$p_e))
  summary(lm(c_and_drivers$emit_resid~c_and_drivers$Temp))

  temp_bury_resid = ggplot(c_and_drivers, aes(x = Temp, y = bury_resid,
                                            color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_point(show.legend = F) +
    geom_text(hjust = 0, nudge_x = .2, nudge_y = .35, show.legend = F, size = 6) +
    theme_classic() +
    ylab(expression(Delta~mol~C~burial~('%')~(P-E)~~residuals)) +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 15,
                                  '2080s' = 17,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  temp_bury_resid

  c_and_drivers$delta_emit_minus_bury = abs((c_and_drivers$Emit-c_and_drivers$Bury) - retro_emit_minus_bury)/retro_emit_minus_bury * 100 * c_and_drivers$emit_bury_change
  c_and_drivers$emit_minus_bury_resid = resid(lm(c_and_drivers$delta_emit_minus_bury~c_and_drivers$p_e))
  summary(lm(c_and_drivers$emit_minus_bury_resid~c_and_drivers$Temp))

  temp_emit_minus_bury_resid = ggplot(c_and_drivers, aes(x = Temp, y = emit_minus_bury_resid,
                                            color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_point(show.legend = F) +
    geom_text(hjust = 0, nudge_x = .2, nudge_y = .4, show.legend = F, size = 6) +
    theme_classic() +
    ylab(expression(Delta~(mol~C~emission-mol~C~burial)~('%')~(P-E)~~residuals)) +
    xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 15,
                                  '2080s' = 17,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  temp_emit_minus_bury_resid

  c_and_drivers$delta_fracRet = abs(c_and_drivers$FracRet*100 - retro_fracRet*100) * c_and_drivers$fracRet_change
  c_and_drivers$fracRet_resid = resid(lm(c_and_drivers$delta_fracRet~c_and_drivers$p_e))
  summary(lm(c_and_drivers$fracRet_resid~c_and_drivers$Temp))

  temp_fracRet_resid = ggplot(c_and_drivers, aes(x = Temp, y = fracRet_resid,
                                               color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Temp, y = fracRet_resid),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid') +
    geom_point(show.legend = F) +
    geom_text(hjust = 0, nudge_x = .1, nudge_y = .1, show.legend = F, size = 6) +
    theme_classic() +
    ylab(expression(Delta~percent~C~removed~('%')~(P-E)~~residuals)) +
    xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 15,
                                  '2080s' = 17,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  temp_fracRet_resid

  c_and_drivers$delta_gpp = abs(c_and_drivers$GPP - retro_gpp)/retro_gpp * 100 * c_and_drivers$gpp_change
  c_and_drivers$delta_gpp_vol = abs(c_and_drivers$GPP_vol - retro_gpp_vol)/retro_gpp_vol * 100 * c_and_drivers$gpp_vol_change
  c_and_drivers$gpp_resid = resid(lm(c_and_drivers$delta_gpp~c_and_drivers$p_e))
  c_and_drivers$gpp_vol_resid = resid(lm(c_and_drivers$delta_gpp_vol~c_and_drivers$p_e))

  temp_gpp_resid = ggplot(c_and_drivers, aes(x = Temp, y = gpp_resid,
                                            color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Temp, y = gpp_resid),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid', show.legend = F)  +
    geom_point(show.legend = F) +
    geom_text(hjust = 0, nudge_x = .2, nudge_y = .8, show.legend = F, size = 6) +
    theme_classic() +
    ylab(expression(Delta~mol~C~GPP~('%')~(P-E)~~residuals)) +
    xlab(expression(Ave.~Annual~Temperature~(degree~C)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 15,
                                  '2080s' = 17,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  temp_gpp_resid

  temp_gpp_vol_resid = ggplot(c_and_drivers, aes(x = Temp, y = gpp_vol_resid,
                                             color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Temp, y = gpp_vol_resid),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid', show.legend = F)  +
    geom_point(show.legend = F) +
    geom_text(hjust = 0, nudge_x = .2, nudge_y = .4, show.legend = F, size = 6) +
    theme_classic() +
    ylab(expression(Delta~mol~C~m^-3~GPP~('%')~(P-E)~~residuals)) +
    xlab(expression(Ave.~Annual~Temperature~(degree~C)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 15,
                                  '2080s' = 17,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  temp_gpp_vol_resid

  c_and_drivers$delta_nep = abs(c_and_drivers$NEP - retro_nep)/abs(retro_nep) * 100 * c_and_drivers$nep_change
  c_and_drivers$nep_resid = resid(lm(c_and_drivers$delta_nep~c_and_drivers$p_e))

  temp_nep_resid = ggplot(c_and_drivers, aes(x = Temp, y = nep_resid,
                                           color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Temp, y = nep_resid),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid', show.legend = F)  +
    geom_point(show.legend = F) +
    geom_text(hjust = 0, nudge_x = .2, nudge_y = .8, show.legend = F, size = 6) +
    theme_classic() +
    ylab(expression(Delta~mol~C~NEP~('%')~(P-E)~~residuals)) +
    xlab(expression(Ave.~Annual~Temperature~(degree~C)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 15,
                                  '2080s' = 17,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  temp_nep_resid

  g = plot_grid(temp_emit_resid, temp_bury_resid, temp_emit_minus_bury_resid, temp_fracRet_resid, temp_gpp_resid, temp_nep_resid,
            labels = c('a', 'b', 'c', 'd', 'e','f'), label_x = .9, nrow = 3, align = 'hv')

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 11, height = 15)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
