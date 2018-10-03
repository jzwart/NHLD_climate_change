fig_frac_increase <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  drivers <- readRDS(drivers_file) # meteo drivers for period / gcm

  all <- readRDS(scenarios) %>%
    dplyr::filter(season == 'all') %>%
    select(Permanent_, period, gcm, Emit, Burial_total, Area, Stage, DOC_Load, HRT, Vol,FracRet, DOC_export,
           waterIn, fluvialOut, epiTemp)

  retro <- all %>%
    dplyr::filter(gcm == 'Retro')

  merged <- dplyr::filter(all, gcm != 'Retro') %>%
    left_join(retro, by = 'Permanent_', suffix = c('_future', '_retro'))

  retro_total <- all %>%
    group_by(period, gcm) %>%
    dplyr::summarise(Emit = sum(Emit),
              Bury = sum(Burial_total),
              Area = sum(Area),
              Water_in = sum(waterIn),
              Vol = sum(Vol),
              FracRet = sum(DOC_Load * FracRet) / sum(DOC_Load),
              DOC_Load = sum(DOC_Load),
              DOC_Export = sum(DOC_export)) %>%
    ungroup() %>%
    dplyr::filter(gcm == 'Retro') %>%
    mutate(tmp = 'a')

  merged_total <- all %>%
    group_by(period, gcm) %>%
    dplyr::summarise(Emit = sum(Emit),
                     Bury = sum(Burial_total),
                     Area = sum(Area),
                     Water_in = sum(waterIn),
                     Vol = sum(Vol),
                     FracRet = sum(DOC_Load * FracRet) / sum(DOC_Load),
                     DOC_Load = sum(DOC_Load),
                     DOC_Export = sum(DOC_export)) %>%
    ungroup() %>%
    dplyr::filter(gcm != 'Retro') %>%
    mutate(tmp = 'a') %>%
    left_join(retro_total, by = 'tmp', suffix = c('_future','_retro')) %>%
    select(-tmp,-period_retro, -gcm_retro)


  emit_ratio = ggplot(merged, aes(x = period_future, y = Emit_future/Emit_retro, color = period_future, fill = period_future,
                     size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_violin() +
    geom_point(data = merged_total, aes(x = period_future, y = Emit_future/Emit_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    theme_classic() +
    ylab(expression(Emissions~Ratio)) +
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, transparent),
                                  '2080s' = t_col(fig_config$period$`2080s`, transparent)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                      values = c('2050s' = 16,
                                 '2080s' = 16),
                      labels = c('2050\'s', '2080\'s'))

  bury_ratio = ggplot(merged, aes(x = period_future, y = Burial_total_future/Burial_total_retro, color = period_future, fill = period_future,
                                  size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_violin() +
    geom_point(data = merged_total, aes(x = period_future, y = Bury_future/Bury_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.07, height = 0, seed = 42)) +
    theme_classic() +
    ylab(expression(Burial~Ratio)) +
    lims(y = c(0,2.5)) +
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, transparent),
                                 '2080s' = t_col(fig_config$period$`2080s`, transparent)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  water_in_ratio = ggplot(merged, aes(x = period_future, y = waterIn_future/waterIn_retro, color = period_future, fill = period_future,
                                  size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_violin() +
    geom_point(data = merged_total, aes(x = period_future, y = Water_in_future/Water_in_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.07, height = 0, seed = 42)) +
    theme_classic() +
    ylab(expression(Water~Load~Ratio)) +
    lims(y = c(0.3,2.1)) +
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, transparent),
                                 '2080s' = t_col(fig_config$period$`2080s`, transparent)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  # ggplot(merged, aes(x = FracRet_future, color = period_future, fill = period_future,
  #                    size =period_future, shape = period_future)) +
  #   geom_histogram() +
  #   theme_classic() +
  #   ylab(expression(count)) +
  #   xlab(expression(Fraction~C~Retained)) +
  #   geom_vline(data = merged_total, aes(xintercept = FracRet_future, color = period_future),
  #              linetype = 'dashed', size = 1) +
  #   # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title.y = element_text(size = 16),
  #         legend.title = element_blank(),
  #         legend.position = c(.15,.85),
  #         legend.text = element_text(size = 12)) +
  #   scale_color_manual(name = 'period_future',
  #                      values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
  #                                 '2080s' = t_col(fig_config$period$`2080s`, 0)),
  #                      labels = c('2050\'s', '2080\'s')) +
  #   scale_fill_manual(name = 'period_future',
  #                     values = c('2050s' = t_col(fig_config$period$`2050s`, transparent),
  #                                '2080s' = t_col(fig_config$period$`2080s`, transparent)),
  #                     labels = c('2050\'s', '2080\'s')) +
  #   scale_size_manual(name = 'period_future',
  #                     values = c('2050s' = 1,
  #                                '2080s' = 1),
  #                     labels = c('2050\'s', '2080\'s')) +
  #   scale_shape_manual(name = 'period_future',
  #                      values = c('2050s' = 16,
  #                                 '2080s' = 16),
  #                      labels = c('2050\'s', '2080\'s'))

  g = ggdraw() +
    draw_plot(emit_ratio, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(bury_ratio, x= .5, y= .5, width = .5, height = .5) +
    draw_plot(water_in_ratio, x = 0, y = 0, width = .5, height = .5)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 11, height = 10)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
