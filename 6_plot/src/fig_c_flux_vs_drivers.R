fig_c_flux_vs_drivers <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  drivers <- readRDS(drivers_file) # meteo drivers for period / gcm

  all <- readRDS(scenarios) %>%
    dplyr::filter(season == 'all') %>%
    mutate(Emit = Emit * 12 * 365, # emissions in g C / year
           Bury = Burial_total * 12 * 365, # bury in g C / year
           Emit_areal = Emit / Area, # emissions in g C/ m2/ year
           Bury_areal = Bury / Area, #  bury in g C / m2 / year
           DOC_load = DOC_Load / Area,
           # R_B = (SWin + Baseflow) / Area_m2,
           Precip_lake = DirectP / Area) %>%
    select(Permanent_, period, gcm, Emit, Bury, Emit_areal, Bury_areal, Area, DOC_load, HRT, Stage, Vol, doc_conc,FracRet, DOC_Load,
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp)

  total <- all %>%
    group_by(period, gcm) %>%
    dplyr::summarise(Emit = sum(Emit),
              Bury = sum(Bury),
              Area = sum(Area),
              Emit_areal = mean(Emit_areal), # mean for all lakes
              Bury_areal = mean(Bury_areal),
              DOC_load = median(DOC_load),
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
              DOC = median(doc_conc)) %>%
    ungroup()

  ave_drivers <- drivers %>%
    group_by(period, gcm, var) %>%
    dplyr::summarise(var_value = sum(var_value)) %>%
    ungroup() %>%
    spread(key = 'var', value = 'var_value')

  c_and_drivers <- left_join(total, ave_drivers, by = c('period' = 'period', 'gcm' = 'gcm')) %>%
    mutate(period = factor(period, levels = c('Retro', '2050s', '2080s')))

  retro_emit = c_and_drivers$Emit[c_and_drivers$gcm =='Retro']
  retro_bury = c_and_drivers$Bury[c_and_drivers$gcm =='Retro']
  retro_emit_minus_bury = retro_emit-retro_bury
  retro_fracRet = c_and_drivers$FracRet[c_and_drivers$gcm == 'Retro']
  retro_doc_load = c_and_drivers$DOC_Load[c_and_drivers$gcm == 'Retro']

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
                                                                    TRUE ~ -1))

  r_b_emit = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = Emit / 10^9, color = period, size =period, shape = period)) +  # converting to gigagrams
    geom_point() +
    theme_classic() +
    ylab(expression(Total~Emissions~(Gg~C~year^-1))) +
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
                      values = c('2050s' = 16,
                                 '2080s' = 16,
                                 'Retro' = 16),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    geom_smooth(aes(x = Runoff_and_baseflow, y = Emit / 10^9), method = 'lm', se = F, color = 'grey60',
                inherit.aes = F, size = 2, linetype = 'dashed')

  r_b_emit

  p_e_emit_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(Emit - retro_emit)/retro_emit * 100 * emit_change,
                                       color = period, size =period, shape = period)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Runoff_and_baseflow, y = abs(Emit - retro_emit)/retro_emit * 100 * emit_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid')  +
    geom_point() +
    theme_classic() +
    ylab(expression(Delta~Emissions~('%'))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  p_e_emit_perc

  r_b_bury = ggplot(c_and_drivers, aes(x = (Precip-Evap), y = Bury / 10^9, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~C~Burial~(Gg~C~year^-1))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))+
    geom_smooth(aes(x = Runoff_and_baseflow, y = Bury / 10^9), method = 'lm', se = F, color = 'grey60',
                inherit.aes = F, size = 2, linetype = 'dashed')

  p_e_bury_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(Bury - retro_bury)/retro_bury * 100 * bury_change,
                                            color = period, size =period, shape = period)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Runoff_and_baseflow, y = abs(Bury - retro_bury)/retro_bury * 100 * bury_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid') +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Burial~('%'))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  p_e_bury_perc

  r_b_emit_to_bury = ggplot(c_and_drivers, aes(x = (Precip-Evap), y = Emit / Bury, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Emissions~to~Burial~(C:C))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))+
    geom_smooth(aes(x = Runoff_and_baseflow, y = Emit / Bury), method = 'lm', se = F, color = 'grey60',
                inherit.aes = F, size = 2, linetype = 'dashed')

  r_b_emit_minus_bury = ggplot(c_and_drivers, aes(x = (Precip-Evap), y = (Emit - Bury)/10^9, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Emissions~minus~Burial~(Gg~C~year^-1))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))+
    geom_smooth(aes(x = Runoff_and_baseflow, y = (Emit - Bury) / 10^9), method = 'lm', se = F, color = 'grey60',
                inherit.aes = F, size = 2, linetype = 'dashed')

  p_e_emit_minus_bury_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs((Emit-Bury) - retro_emit_minus_bury)/retro_emit_minus_bury * 100 * emit_bury_change,
                                            color = period, size =period, shape = period)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Runoff_and_baseflow, y = abs((Emit-Bury) - retro_emit_minus_bury)/retro_emit_minus_bury * 100 * emit_bury_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid') +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~(Emission-Burial)~('%'))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  p_e_emit_minus_bury_perc

  doc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = DOC, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(DOC~(mg~C~L^-1))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))+
    geom_smooth(aes(x = Runoff_and_baseflow, y = DOC), method = 'lm', se = F, color = 'grey60',
                inherit.aes = F, size = 2, linetype = 'dashed')
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

  doc_load = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = DOC_load, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(DOC~Load)) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))+
    geom_smooth(aes(x = Runoff_and_baseflow, y = DOC_load), method = 'lm', se = F, color = 'grey60',
                inherit.aes = F, size = 2, linetype = 'dashed')

  p_e_fracRet_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(FracRet*100 - retro_fracRet*100) * fracRet_change,
                                               color = period, size =period, shape = period)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Runoff_and_baseflow, y = abs(FracRet*100 - retro_fracRet*100) * fracRet_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid') +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Percent~C~Removed~('%'))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  p_e_fracRet_perc

  p_e_doc_load_perc = ggplot(c_and_drivers, aes(x = (Precip - Evap), y = abs(DOC_Load - retro_doc_load)/retro_doc_load * 100 * doc_load_change,
                                               color = period, size =period, shape = period)) +  # converting to %
    geom_hline(yintercept = 0, linetype = 'dashed', color ='grey60', size = 1.5) +
    geom_smooth(aes(x = Runoff_and_baseflow, y = abs(DOC_Load - retro_doc_load)/retro_doc_load * 100 * doc_load_change),
                method = 'lm', se = F, color = 'black',
                inherit.aes = F, size = 2, linetype = 'solid') +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~C~Load~('%'))) +
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
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  p_e_doc_load_perc

  # g = ggdraw() +
  #   draw_plot(p_e_emit_perc, x = 0, y = .5, width = .5, height = .5) +
  #   draw_plot(p_e_bury_perc, x= .5, y= .5, width = .5, height = .5) +
  #   draw_plot(p_e_emit_minus_bury_perc, x = 0, y = 0, width = .5, height = .5) +
  #   draw_plot(p_e_fracRet_perc, x= .5, y= 0, width = .5, height = .5)

  g = plot_grid(p_e_emit_perc, p_e_bury_perc, p_e_emit_minus_bury_perc, p_e_fracRet_perc,
            labels = c('A', 'B', 'C', 'D'), nrow = 2, align = 'hv')

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 11, height = 10)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
