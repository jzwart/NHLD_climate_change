
fig_relative_contribution <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){
  # figure for the change in emissions / burial of top x% of emitting lakes

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
    select(Permanent_, period, gcm, Emit, Bury, Emit_areal, Bury_areal, Area, DOC_load, HRT, Stage, Vol, doc_conc,emergent_d_epi,pH,
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp, lakeSizeBins, percentEvap, GPP, Vepi, dicLoadvResp, FracRet, fracCO2)

  total <- all %>%
    group_by(period, gcm) %>%
    dplyr::summarise(Emit = sum(Emit),
                     Bury = sum(Bury),
                     Area = sum(Area),
                     Emit_areal = mean(Emit_areal), # mean for all lakes
                     Bury_areal = mean(Bury_areal),
                     DOC_load = mean(DOC_load),
                     Precip_lake = mean(Precip_lake),
                     Water_in = sum(waterIn),
                     # R_B = median(R_B, na.rm = T),
                     ndays_ice = mean(ndays_ice),
                     epiTemp = median(epiTemp),
                     HRT = median(HRT),
                     Vol = sum(Vol),
                     DOC = median(doc_conc),
                     GPP = sum(GPP)) %>%
    ungroup()

  ave_drivers <- drivers %>%
    group_by(period, gcm, var) %>%
    dplyr::summarise(var_value = sum(var_value)) %>%
    ungroup() %>%
    spread(key = 'var', value = 'var_value')

  c_and_drivers <- left_join(total, ave_drivers, by = c('period' = 'period', 'gcm' = 'gcm')) %>%
    mutate(period = factor(period, levels = c('Retro', '2050s', '2080s')))

  region_retro <- c_and_drivers %>%
    dplyr::filter(gcm == 'Retro') %>%
    mutate(tmp = 'a')

  region_future <- c_and_drivers %>%
    dplyr::filter(gcm != 'Retro') %>%
    mutate(tmp = 'a') %>%
    left_join(region_retro, by = 'tmp', suffix = c('_future','_retro')) %>%
    select(-tmp)

  stage <- all %>%
    group_by(period, gcm) %>%
    arrange(percentEvap, .by_group = T) %>%
    ungroup() %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm'))

  stage_retro <- stage %>%
    dplyr::filter(gcm == 'Retro')

  stage_future <- stage %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(stage_retro, by = 'Permanent_', suffix = c('_future','_retro')) %>%
    mutate(stage_ratio = Stage_future / Stage_retro,
           stage_diff = Stage_future - Stage_retro,
           area_ratio = Area_future / Area_retro) %>%
    group_by(period_future, gcm_future) %>%
    arrange(percentEvap_retro, .by_group = T) %>%
    ungroup()

  retro <- all %>%
    dplyr::filter(gcm == 'Retro')

  merged <- dplyr::filter(all, gcm != 'Retro') %>%
    left_join(retro, by = 'Permanent_', suffix = c('_future', '_retro'))

  merged$doc_change = merged$doc_conc_future/merged$doc_conc_retro



  stage_future = stage_future %>%
    mutate(Delta_Emit = abs(Emit_future-Emit_retro)/Emit_retro*100 * ifelse(Emit_future > Emit_retro, 1, -1),
           Delta_Emit_areal = abs(Emit_areal_future-Emit_areal_retro)/Emit_areal_retro*100 * ifelse(Emit_areal_future > Emit_areal_retro, 1, -1),
           Delta_Area = abs(Area_future-Area_retro)/Area_retro*100 * ifelse(Area_future > Area_retro, 1, -1),
           Delta_Bury = abs(Bury_future-Bury_retro)/Bury_retro*100 * ifelse(Bury_future > Bury_retro, 1, -1),
           Delta_Bury_areal = abs(Bury_areal_future-Bury_areal_retro)/Bury_areal_retro*100 * ifelse(Bury_areal_future > Bury_areal_retro, 1, -1),
           Delta_GPP = abs(GPP_future*Vepi_future - GPP_retro*Vepi_retro)/(GPP_retro*Vepi_retro) *100*
             ifelse(GPP_future*Vepi_future > GPP_retro*Vepi_retro,1,-1),
           Delta_GPP_vol = abs(GPP_future - GPP_retro)/(GPP_retro) *100*ifelse(GPP_future > GPP_retro,1,-1),
           Delta_Vol = abs(Vepi_future - Vepi_retro)/Vepi_retro*100*ifelse(Vepi_future>Vepi_retro,1,-1))

  plot_data = tibble()

  gcms = unique(stage_future$gcm_future)
  periods = unique(stage_future$period_future)

  for(i in 1:length(gcms)){
    for(j in 1:length(periods)){
      cur <- dplyr::filter(stage_future, gcm_future == gcms[i], period_future == periods[j])

      a = ggplot(cur, aes(y = Delta_Area, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_Area, outfit = loess_area<<-..y..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-60,60))
      print(a)

      e_a = ggplot(cur, aes(y = Delta_Emit_areal, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_Emit_areal, outfit=loess_emit_areal<<-..y..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-60,60))

      print(e_a)

      f = ggplot(cur, aes(y = Delta_Emit_areal, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_Emit_areal, outfit=loess_fhee<<-..x..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-60,60))
      print(f)

      e = ggplot(cur, aes(y = Delta_Emit, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_Emit, outfit=loess_emit<<-..y..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-60,60))
      print(e)

      b = ggplot(cur, aes(y = Delta_Bury, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_Bury, outfit=loess_bury<<-..y..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-60,60))
      print(b)

      b_a = ggplot(cur, aes(y = Delta_Bury_areal, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_Bury_areal, outfit=loess_bury_areal<<-..y..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-60,60))
      print(b_a)

      g = ggplot(cur, aes(y = Delta_GPP, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_GPP, outfit=loess_gpp<<-..y..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-100,100))
      print(g)

      g_v = ggplot(cur, aes(y = Delta_GPP_vol, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_GPP_vol, outfit=loess_gpp_vol<<-..y..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-100,100))
      print(g_v)

      v = ggplot(cur, aes(y = Delta_Vol, x = percentEvap_future)) +
        geom_smooth(aes(y = Delta_Vol, outfit=loess_vol<<-..y..,
                        x = percentEvap_future),
                    linetype = 'dotted',
                    method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
        ylim(c(-100,100))
      print(v)

      out <- data_frame(gcm = gcms[i], period = periods[j], fhee = loess_fhee, emit_areal = loess_emit_areal,
                        emit = loess_emit, area = loess_area, bury = loess_bury, bury_areal = loess_bury_areal,
                        gpp = loess_gpp, gpp_vol = loess_gpp_vol, vol = loess_vol)

      plot_data = bind_rows(plot_data, out)
    }
  }

  plot_data$period_gcm = paste(plot_data$period, plot_data$gcm)

  p_e = c_and_drivers %>%
    select(period, gcm, Precip, Evap)

  plot_data = left_join(plot_data, p_e, by = c('gcm' = 'gcm', 'period' = 'period'))
  #ordering by P-E
  sorted <- plot_data$period_gcm[sort.list(plot_data$Precip-plot_data$Evap)]
  sorted <- as.character(sorted[!duplicated(sorted)])

  plot_data$period_gcm <- factor(plot_data$period_gcm,levels = sorted)

  # what if we only kept driest and wetest scenarios?
  plot_data = plot_data %>%
    mutate(p_e = Precip - Evap)

  max_pe = max(plot_data$p_e)
  min_pe = min(plot_data$p_e)

  plot_data = dplyr::filter(plot_data, p_e %in% c(max_pe, min_pe))

  levels(plot_data$period_gcm) <- c('Dry',rep(NA,10),'Wet')

  emit_facet = ggplot(plot_data, aes(y = emit, x = fhee)) +
    geom_ribbon(aes(x =fhee, ymin = emit_areal + (emit - emit_areal), ymax = emit_areal,
                    fill = 'areal')) +
    geom_ribbon(aes(x =fhee, ymin = (emit - emit_areal), ymax = emit_areal,
                    fill = 'areal')) +
    geom_ribbon(aes(x = fhee, ymin = emit_areal,
                    ymax = rep(0,nrow(plot_data)),
                    fill = 'area')) +
    geom_hline(yintercept = 0, size =1, linetype = 'dashed', color = 'grey40') +
    geom_line(aes(color = 'total'), size = 2)+
    theme_classic()+
    ylab(expression(Delta~Areal~or~Total~Emissions~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.text.x = element_text(size = 16),
          axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14),
          legend.spacing = unit(0, 'lines'),
          strip.background = element_blank(),
          strip.text = element_text(size = 20),
          panel.spacing = unit(2,'lines'))+
    facet_wrap(~period_gcm) +
    ylim(c(-40,50))+
    scale_y_continuous(sec.axis = sec_axis(~., name = expression(Delta~Lake~Area~('%')))) +
    scale_fill_manual(values = c('areal' = wesanderson::wes_palettes$Zissou1[2],
                                 'area' = wesanderson::wes_palettes$Zissou1[3]),
                      labels = c('Areal Emissions', 'Lake Area'))+
    scale_color_manual(values = c('total' = 'black'),
                       labels = 'Total Emissions')

  emit_facet

  bury_facet = ggplot(plot_data, aes(y = bury, x = fhee)) +
    geom_ribbon(aes(x =fhee, ymin = bury_areal + (bury - bury_areal), ymax = bury_areal,
                fill = 'areal')) +
    geom_ribbon(aes(x =fhee, ymin = (bury - bury_areal), ymax = bury_areal,
                fill = 'areal')) +
    geom_ribbon(aes(x = fhee, ymin = bury_areal,
                    ymax = rep(0,nrow(plot_data)),
                fill = 'area')) +
    geom_hline(yintercept = 0, size =1, linetype = 'dashed', color = 'grey40') +
    geom_line(aes(color = 'total'), size = 2)+
    theme_classic()+
    ylab(expression(Delta~Areal~or~Total~Burial~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.text.x = element_text(size = 16),
          axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14),
          legend.spacing = unit(0,'lines'),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(2,'lines'))+
    facet_wrap(~period_gcm)+
    ylim(c(-40,25))+
    scale_y_continuous(sec.axis = sec_axis(~., name = expression(Delta~Lake~Area~('%')))) +
    scale_fill_manual(values = c('areal' = wesanderson::wes_palettes$Zissou1[2],
                                 'area' = wesanderson::wes_palettes$Zissou1[3]),
                      labels = c('Areal Burial', 'Lake Area'))+
    scale_color_manual(values = c('total' = 'black'),
                       labels = 'Total Burial')

  bury_facet


  gpp_facet = ggplot(plot_data, aes(y = gpp, x = fhee)) +
    geom_ribbon(aes(x =fhee, ymin = gpp_vol + (gpp - gpp_vol), ymax = gpp_vol,
                fill = 'volumetric')) +
    geom_ribbon(aes(x =fhee, ymin = (gpp - gpp_vol), ymax = gpp_vol,
                fill = 'volumetric')) +
    geom_ribbon(aes(x = fhee, ymin = gpp_vol,
                    ymax = rep(0,nrow(plot_data)),
                fill = 'volume')) +
    geom_hline(yintercept = 0, size =1, linetype = 'dashed', color = 'grey40') +
    geom_line(aes(color = 'total'), size = 2)+
    theme_classic()+
    ylab(expression(Delta~Volumetric~or~Total~GPP~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.text.x = element_text(size = 16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.8,.2),
          legend.background = element_blank(),
          legend.text = element_text(size = 14),
          legend.spacing = unit(0,'lines'),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.spacing = unit(2,'lines'))+
    facet_wrap(~period_gcm)+
    ylim(c(-60,30))+
    scale_y_continuous(sec.axis = sec_axis(~., name = expression(Delta~Epilimnion~Volume~('%')))) +
    scale_fill_manual(values = c('volumetric' = wesanderson::wes_palettes$Zissou1[2],
                                 'volume' = wesanderson::wes_palettes$Zissou1[3]),
                      labels = c('Volumetric GPP', 'Epilimnion Volume'))+
    scale_color_manual(values = c('total' = 'black'),
                       labels = 'Total GPP')

  gpp_facet

  g = plot_grid(emit_facet, bury_facet, gpp_facet,
                labels = c('a', 'b', 'c'), align = 'hv', rows = 3)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 9, height = 21)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}


