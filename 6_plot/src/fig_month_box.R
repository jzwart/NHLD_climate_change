fig_month_box <- function(fig_ind, vars_ind_file, vars_yml, fig_cfg_yml, var_lookup_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  var_lookup <- yaml::yaml.load_file(var_lookup_yml) # contains fig labels and units

  vars <- noquote(yaml::yaml.load_file(vars_yml)$var) # indicates which periods, seasons, and variable we want returned

  monthly_ave <- readRDS(sc_retrieve(vars_ind_file, remake_file = remake_file))

  watersheds<-read.table('1_data/in/NHLDsheds_20170323.txt',
                         stringsAsFactors = F,
                         header=T,
                         sep = '\t')

  monthly_ave <- monthly_ave %>%
    left_join(watersheds, by = 'Permanent_') %>%
    dplyr::filter(!is.na(Area_m2))

  # vars that we want plotted by watershed area or lake area
  monthly_ave <- monthly_ave %>%
    select(Permanent_, gcm, period, month, eval(vars)) %>%
    gather(key = 'var', value = 'month_mean', eval(vars)) %>% # turning into long format for plotting
    group_by(month, period, gcm, var) %>%
    summarise(med_all = median(month_mean)) %>%
    ungroup() %>%
    group_by(month, var, period) %>%
    summarise(med = median(med_all),
              min = min(med_all),
              max = max(med_all)) %>%
    ungroup() %>%
    mutate(month = as.Date(paste('2001',month,'01',sep='-'))) # creating date so it can plot on x-axis

  units <- var_lookup$units[vars] %>%
    bind_rows() %>% as.character()
  units <- paste(vars, '\n',units, sep ='')
  names(units) <- vars

  # box plot data
  vars =
  var_results <- readRDS(sc_retrieve(var_ind_file, remake_file = remake_file))

  y <- noquote(var_lookup$ratio_var[var][[1]]) # what we want to plot

  ratio <- var_results %>%
    select(Permanent_, period, season, starts_with('ratio')) %>%
    dplyr::filter(var_results[[y]] < 6) # removing crazy outliers


  g = ggplot(monthly_ave, aes(x = month, y = med, color = period, size = period, linetype = period)) +
    geom_line() +
    geom_ribbon(data = dplyr::filter(monthly_ave, period != 'Retro'),
                aes(x = month, y = med, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    theme_classic() +
    ylab(NULL) +
    facet_wrap(~var, scales = 'free_y', labeller = labeller(var = units),
               strip.position = 'left') +
    theme(strip.background = element_blank(),
          strip.placement = 'outside') +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050s', '2080s', 'Retro')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050s', '2080s', 'Retro')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 2,
                                 '2080s' = 2,
                                 'Retro' = 1.1),
                      labels = c('2050s', '2080s', 'Retro')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050s', '2080s', 'Retro')) +
    scale_x_date(labels = scales::date_format('%b'))

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 18, height = 11)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
