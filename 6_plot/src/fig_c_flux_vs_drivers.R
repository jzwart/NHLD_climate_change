fig_c_flux_vs_drivers <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){

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
           doc_conc,FracRet, DOC_Load, GPP, Vepi, DOC_Respired, sed_resp, Vepi, tp_load,
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
              Sed_resp = sum(sed_resp),
              TP_Load = sum(tp_load)) %>%
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
  retro_tp_load = c_and_drivers$TP_Load[c_and_drivers$gcm == 'Retro']

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
                          tp_load_change = case_when(TP_Load > retro_tp_load ~ 1,
                                                     TRUE ~ -1),
                          gcm_label = case_when(gcm == 'CESM1_CAM5' ~ 3,
                                                gcm == 'FIO_ESM' ~ 4,
                                                gcm == 'GFDL_CM3' ~ 2,
                                                gcm == 'GFDL_ESM2M' ~ 6,
                                                gcm == 'HadGEM2_AO' ~ 1,
                                                gcm == 'HadGEM2_CC' ~ 5))

  # r_b_emit = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = Emit / 10^9, color = period, size =period, shape = period)) +  # converting to gigagrams
  #   geom_point() +
  #   theme_classic() +
  #   ylab(expression(Total~Emissions~(Gg~C~year^-1))) +
  #   # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title.y = element_text(size = 16),
  #         axis.title.x = element_blank(),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.8),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period',
  #                      values = c('2050s' = fig_config$period$`2050s`,
  #                                 '2080s' = fig_config$period$`2080s`,
  #                                 'Retro' = fig_config$period$Retro),
  #                      labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period',
  #                     values = c('2050s' = 8,
  #                                '2080s' = 8,
  #                                'Retro' = 8),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period',
  #                     values = c('2050s' = 16,
  #                                '2080s' = 16,
  #                                'Retro' = 16),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   geom_smooth(aes(x = Runoff_and_baseflow, y = Emit / 10^9), method = 'lm', se = F, color = 'grey60',
  #               inherit.aes = F, size = 2, linetype = 'dashed')
  #
  # r_b_emit

  p_e_emit_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(Emit - retro_emit)/retro_emit * 100 * emit_change,
                                       color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = (Precip - Evap), y = abs(Emit - retro_emit)/retro_emit * 100 * emit_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid')  +
    geom_point() +
    # geom_text(hjust = 0, nudge_x = 4, nudge_y = 1, show.legend = F, size = 6) +
    geom_text(position = position_dodge2(width = 5),  show.legend = F, size = 5, color = 'black') +

    theme_classic() +
    ylab(expression(Delta~mol~C~emissions~('%'))) +
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
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

  p_e_emit_perc
  delta_emit = abs(c_and_drivers$Emit - retro_emit)/retro_emit * 100 * c_and_drivers$emit_change
  p_e = c_and_drivers$Precip - c_and_drivers$Evap
  temp = c_and_drivers$Temp

  summary(lm(delta_emit~p_e))
  summary(lm(delta_emit~p_e+temp))
  res = resid(lm(delta_emit~p_e))
  summary(lm(res~temp))
  plot(res~temp, pch = 16)

  # r_b_bury = ggplot(c_and_drivers, aes(x = (Precip-Evap), y = Bury / 10^9, color = period)) +  # converting to gigagrams
  #   geom_point(size = 8, shape = 16, show.legend = F) +
  #   theme_classic() +
  #   ylab(expression(Total~C~Burial~(Gg~C~year^-1))) +
  #   # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title.y = element_text(size = 16),
  #         axis.title.x = element_blank(),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.8),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period',
  #                      values = c('2050s' = fig_config$period$`2050s`,
  #                                 '2080s' = fig_config$period$`2080s`,
  #                                 'Retro' = fig_config$period$Retro),
  #                      labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period',
  #                     values = c('2050s' = 8,
  #                                '2080s' = 8,
  #                                'Retro' = 8),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period',
  #                      values = c('2050s' = 16,
  #                                 '2080s' = 16,
  #                                 'Retro' = 16),
  #                      labels = c('Historic','2050\'s', '2080\'s'))+
  #   geom_smooth(aes(x = Runoff_and_baseflow, y = Bury / 10^9), method = 'lm', se = F, color = 'grey60',
  #               inherit.aes = F, size = 2, linetype = 'dashed')

  p_e_bury_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(Bury - retro_bury)/retro_bury * 100 * bury_change,
                                            color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = (Precip - Evap), y = abs(Bury - retro_bury)/retro_bury * 100 * bury_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid') +
    geom_point(show.legend = F) +
    # geom_text(hjust = 0, nudge_x = 6, nudge_y = .5, show.legend = F, size = 6) +
    geom_text(position = position_dodge2(width = 4),  show.legend = F, size = 5, color = 'black') +

    theme_classic() +
    ylab(expression(Delta~mol~C~burial~('%'))) +
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
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

  p_e_bury_perc
  delta_bury = abs(c_and_drivers$Bury - retro_bury)/retro_bury * 100 * c_and_drivers$bury_change

  summary(lm(delta_bury~p_e))
  summary(lm(delta_bury~p_e+temp))
  res = resid(lm(delta_bury~p_e))
  summary(lm(res~temp))
  plot(res~temp, pch = 16)

  # r_b_emit_to_bury = ggplot(c_and_drivers, aes(x = (Precip-Evap), y = Emit / Bury, fill = period, color = period)) +  # converting to gigagrams
  #   geom_point(size = 8, shape = 16, show.legend = F) +
  #   theme_classic() +
  #   ylab(expression(Emissions~to~Burial~(C:C))) +
  #   xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title = element_text(size = 16),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.8),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period',
  #                      values = c('2050s' = fig_config$period$`2050s`,
  #                                 '2080s' = fig_config$period$`2080s`,
  #                                 'Retro' = fig_config$period$Retro),
  #                      labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period',
  #                     values = c('2050s' = 8,
  #                                '2080s' = 8,
  #                                'Retro' = 8),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period',
  #                      values = c('2050s' = 16,
  #                                 '2080s' = 16,
  #                                 'Retro' = 16),
  #                      labels = c('Historic','2050\'s', '2080\'s'))+
  #   geom_smooth(aes(x = Runoff_and_baseflow, y = Emit / Bury), method = 'lm', se = F, color = 'grey60',
  #               inherit.aes = F, size = 2, linetype = 'dashed')

  # r_b_emit_minus_bury = ggplot(c_and_drivers, aes(x = (Precip-Evap), y = (Emit - Bury)/10^9, fill = period, color = period)) +  # converting to gigagrams
  #   geom_point(size = 8, shape = 16, show.legend = F) +
  #   theme_classic() +
  #   ylab(expression(Emissions~minus~Burial~(Gg~C~year^-1))) +
  #   xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title = element_text(size = 16),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.8),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period',
  #                      values = c('2050s' = fig_config$period$`2050s`,
  #                                 '2080s' = fig_config$period$`2080s`,
  #                                 'Retro' = fig_config$period$Retro),
  #                      labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period',
  #                     values = c('2050s' = 8,
  #                                '2080s' = 8,
  #                                'Retro' = 8),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period',
  #                      values = c('2050s' = 16,
  #                                 '2080s' = 16,
  #                                 'Retro' = 16),
  #                      labels = c('Historic','2050\'s', '2080\'s'))+
  #   geom_smooth(aes(x = Runoff_and_baseflow, y = (Emit - Bury) / 10^9), method = 'lm', se = F, color = 'grey60',
  #               inherit.aes = F, size = 2, linetype = 'dashed')

  p_e_emit_minus_bury_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs((Emit-Bury) - retro_emit_minus_bury)/retro_emit_minus_bury * 100 * emit_bury_change,
                                            color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = (Precip - Evap), y = abs((Emit-Bury) - retro_emit_minus_bury)/retro_emit_minus_bury * 100 * emit_bury_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid') +
    geom_point(show.legend = F) +
    # geom_text(hjust = 0, nudge_x = 4, nudge_y = 1, show.legend = F, size = 6) +
    geom_text(position = position_dodge2(width = 4),  show.legend = F, size = 5, color = 'black') +

    theme_classic() +
    ylab(expression(Delta~(mol~C~emission-mol~C~burial)~('%'))) +
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

  p_e_emit_minus_bury_perc
  delta_emit_minus_bury = abs((c_and_drivers$Emit-c_and_drivers$Bury) - retro_emit_minus_bury)/retro_emit_minus_bury * 100 * c_and_drivers$emit_bury_change

  summary(lm(delta_emit_minus_bury~p_e))
  summary(lm(delta_emit_minus_bury~p_e+temp))
  res = resid(lm(delta_emit_minus_bury~p_e))
  summary(lm(res~temp))
  plot(res~temp, pch = 16)

  # doc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = DOC, fill = period, color = period)) +  # converting to gigagrams
  #   geom_point(size = 8, shape = 16, show.legend = F) +
  #   theme_classic() +
  #   ylab(expression(DOC~(mg~C~L^-1))) +
  #   xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title = element_text(size = 16),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.8),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period',
  #                      values = c('2050s' = fig_config$period$`2050s`,
  #                                 '2080s' = fig_config$period$`2080s`,
  #                                 'Retro' = fig_config$period$Retro),
  #                      labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period',
  #                     values = c('2050s' = 8,
  #                                '2080s' = 8,
  #                                'Retro' = 8),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period',
  #                      values = c('2050s' = 16,
  #                                 '2080s' = 16,
  #                                 'Retro' = 16),
  #                      labels = c('Historic','2050\'s', '2080\'s'))+
  #   geom_smooth(aes(x = Runoff_and_baseflow, y = DOC), method = 'lm', se = F, color = 'grey60',
  #               inherit.aes = F, size = 2, linetype = 'dashed')
#
#   ggplot(c_and_drivers, aes(x = (Precip - Evap), y = regional_DOC, fill = period, color = period)) +  # converting to gigagrams
#     geom_point(size = 8, shape = 16, show.legend = F) +
#     theme_classic() +
#     ylab(expression(DOC~(mg~C~L^-1))) +
#     xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
#     theme(axis.text = element_text(size=16),
#           axis.title = element_text(size = 16),
#           legend.title = element_blank(),
#           legend.position = c(.15,.8),
#           legend.text = element_text(size = 12)) +
#     scale_color_manual(name = 'period',
#                        values = c('2050s' = fig_config$period$`2050s`,
#                                   '2080s' = fig_config$period$`2080s`,
#                                   'Retro' = fig_config$period$Retro),
#                        labels = c('Historic','2050\'s', '2080\'s')) +
#     scale_size_manual(name = 'period',
#                       values = c('2050s' = 8,
#                                  '2080s' = 8,
#                                  'Retro' = 8),
#                       labels = c('Historic','2050\'s', '2080\'s')) +
#     scale_shape_manual(name = 'period',
#                        values = c('2050s' = 16,
#                                   '2080s' = 16,
#                                   'Retro' = 16),
#                        labels = c('Historic','2050\'s', '2080\'s'))+
#     geom_smooth(aes(x = Runoff_and_baseflow, y = regional_DOC), method = 'lm', se = F, color = 'grey60',
#                 inherit.aes = F, size = 2, linetype = 'dashed')

  # doc_load = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = DOC_load, fill = period, color = period)) +  # converting to gigagrams
  #   geom_point(size = 8, shape = 16, show.legend = F) +
  #   theme_classic() +
  #   ylab(expression(DOC~Load)) +
  #   xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title = element_text(size = 16),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.8),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period',
  #                      values = c('2050s' = fig_config$period$`2050s`,
  #                                 '2080s' = fig_config$period$`2080s`,
  #                                 'Retro' = fig_config$period$Retro),
  #                      labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period',
  #                     values = c('2050s' = 8,
  #                                '2080s' = 8,
  #                                'Retro' = 8),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period',
  #                      values = c('2050s' = 16,
  #                                 '2080s' = 16,
  #                                 'Retro' = 16),
  #                      labels = c('Historic','2050\'s', '2080\'s'))+
  #   geom_smooth(aes(x = Runoff_and_baseflow, y = DOC_load), method = 'lm', se = F, color = 'grey60',
  #               inherit.aes = F, size = 2, linetype = 'dashed')
  # tp_load = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(TP_Load - retro_tp_load)/retro_tp_load * 100 * tp_load_change,
  #                                     fill = period, color = period)) +
  #   geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
  #   geom_point(size = 8, shape = 16, show.legend = F) +
  #   theme_classic() +
  #   ylab(expression(TP~Load)) +
  #   xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title = element_text(size = 16),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.8),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period',
  #                      values = c('2050s' = fig_config$period$`2050s`,
  #                                 '2080s' = fig_config$period$`2080s`,
  #                                 'Retro' = fig_config$period$Retro),
  #                      labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period',
  #                     values = c('2050s' = 8,
  #                                '2080s' = 8,
  #                                'Retro' = 8),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period',
  #                      values = c('2050s' = 16,
  #                                 '2080s' = 16,
  #                                 'Retro' = 16),
  #                      labels = c('Historic','2050\'s', '2080\'s'))+
  #   geom_smooth(aes(x = Runoff_and_baseflow, y = abs(TP_Load - retro_tp_load)/retro_tp_load * 100 * tp_load_change),
  #               method = 'lm', se = F, color = 'grey60',
  #               inherit.aes = F, size = 2, linetype = 'dashed')

  p_e_fracRet_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(FracRet*100 - retro_fracRet*100) * fracRet_change,
                                               color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = (Precip - Evap), y = abs(FracRet*100 - retro_fracRet*100) * fracRet_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid') +
    geom_point(show.legend = F) +
    # geom_text(hjust = 0, nudge_x = 5, nudge_y = .15, show.legend = F, size = 6) +
    geom_text(position = position_dodge2(width = 4),  show.legend = F, size = 5, color = 'black') +

    theme_classic() +
    ylab(expression(Delta~percent~C~removed~('%'))) +
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

  p_e_fracRet_perc
  delta_fracRet = abs(c_and_drivers$FracRet*100 - retro_fracRet*100) * c_and_drivers$fracRet_change

  summary(lm(delta_fracRet~p_e))
  summary(lm(delta_fracRet~p_e+temp))
  res = resid(lm(delta_fracRet~p_e))
  summary(lm(res~temp))
  plot(res~temp, pch = 16)

  # p_e_doc_load_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(DOC_Load - retro_doc_load)/retro_doc_load * 100 * doc_load_change,
  #                                              color = period, size =period, shape = period)) +  # converting to %
  #   geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
  #   geom_smooth(aes(x = Runoff_and_baseflow, y = abs(DOC_Load - retro_doc_load)/retro_doc_load * 100 * doc_load_change),
  #               method = 'lm', se = F, color = 'black',
  #               inherit.aes = F, size = 2, linetype = 'solid') +
  #   geom_point(show.legend = F) +
  #   theme_classic() +
  #   ylab(expression(Delta~C~Load~('%'))) +
  #   # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title.y = element_text(size = 16),
  #         axis.title.x = element_blank(),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.8),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period',
  #                      values = c('2050s' = fig_config$period$`2050s`,
  #                                 '2080s' = fig_config$period$`2080s`,
  #                                 'Retro' = fig_config$period$Retro),
  #                      labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period',
  #                     values = c('2050s' = 8,
  #                                '2080s' = 8,
  #                                'Retro' = 8),
  #                     labels = c('Historic','2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period',
  #                      values = c('2050s' = 15,
  #                                 '2080s' = 17,
  #                                 'Retro' = 16),
  #                      labels = c('Historic','2050\'s', '2080\'s'))
  #
  # p_e_doc_load_perc
  p_e_gpp_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(GPP - retro_gpp)/retro_gpp * 100 * gpp_change,
                                            color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = (Precip-Evap), y = abs(GPP - retro_gpp)/retro_gpp * 100 * gpp_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid', show.legend = F)  +
    geom_point(show.legend = F) +
    # geom_text(hjust = 0, nudge_x = 4, nudge_y = 1, show.legend = F, size = 6) +
    geom_text(position = position_dodge2(width = 4),  show.legend = F, size = 5, color = 'black') +

    theme_classic() +
    ylab(expression(Delta~mol~C~GPP~('%'))) +
    xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
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

  p_e_gpp_perc
  delta_gpp = abs(c_and_drivers$GPP - retro_gpp)/retro_gpp * 100 * c_and_drivers$gpp_change
  delta_gpp_vol = abs(c_and_drivers$GPP_vol - retro_gpp_vol)/retro_gpp_vol * 100 * c_and_drivers$gpp_vol_change

  # total GPP
  summary(lm(delta_gpp~p_e))
  summary(lm(delta_gpp~p_e+temp))
  res = resid(lm(delta_gpp~p_e))
  summary(lm(res~temp))
  cor(res, temp)
  plot(res~temp, pch = 16)

  # volumetric GPP
  summary(lm(delta_gpp_vol~p_e))
  summary(lm(delta_gpp_vol~p_e+temp))
  res = resid(lm(delta_gpp_vol~p_e))
  summary(lm(res~temp))
  cor(res,temp)
  plot(res~temp, pch = 16)

  p_e_nep_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(NEP - retro_nep)/abs(retro_nep) * 100 * nep_change,
                                           color = period, size =period, shape = period, label = gcm_label)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = (Precip-Evap), y = abs(NEP - retro_nep)/abs(retro_nep) * 100 * nep_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid', show.legend = F)  +
    geom_point(show.legend = F) +
    # geom_text(hjust = 0, nudge_x = 4, nudge_y = 1, show.legend = F, size = 6) +
    geom_text(position = position_dodge2(width = 4),  show.legend = F, size = 5, color = 'black') +
    theme_classic() +
    ylab(expression(Delta~mol~C~NEP~('%'))) +
    xlab(expression(Precipitation-Evapotranspiration~(mm~year^-1)))+
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

  p_e_nep_perc
  delta_nep = abs(c_and_drivers$NEP - retro_nep)/abs(retro_nep) * 100 * c_and_drivers$nep_change

  summary(lm(delta_nep~p_e))
  summary(lm(delta_nep~p_e+temp))
  res = resid(lm(delta_nep~p_e))
  summary(lm(res~temp))
  cor(res, temp)
  plot(res~temp, pch = 16)

  # g = ggdraw() +
  #   draw_plot(p_e_emit_perc, x = 0, y = .5, width = .5, height = .5) +
  #   draw_plot(p_e_bury_perc, x= .5, y= .5, width = .5, height = .5) +
  #   draw_plot(p_e_emit_minus_bury_perc, x = 0, y = 0, width = .5, height = .5) +
  #   draw_plot(p_e_fracRet_perc, x= .5, y= 0, width = .5, height = .5)

  g = plot_grid(p_e_emit_perc, p_e_bury_perc, p_e_emit_minus_bury_perc, p_e_fracRet_perc, p_e_gpp_perc, p_e_nep_perc,
            labels = c('a', 'b', 'c', 'd', 'e','f'), nrow = 3, align = 'hv')

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 11, height = 15)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
