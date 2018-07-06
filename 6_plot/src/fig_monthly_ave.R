fig_monthly_ave <- function(fig_ind, vars_ind_file, vars_yml, fig_cfg_yml, var_lookup_yml, remake_file, gd_config){
  # read in data and make average response of variable by month; error bars are range of gcms (max / min)

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  var_lookup <- yaml::yaml.load_file(var_lookup_yml) # contains fig labels and units

  vars <- noquote(yaml::yaml.load_file(vars_yml)$var) # indicates which periods, seasons, and variable we want returned

  more_vars <- c('sw_in_wa', 'baseflow_wa',  'sw_in_la', 'baseflow_la', 'precip_la', 'evap_la', 'bury_la',
    'emit_la', 'gpp_la', 'doc_resp_la', 'gw_in_la', 'doc_loads_la', 'tp_loads_la', 'hrt', 'gw_out_la', 'sw_out_la')

  vars <- c(vars, noquote(more_vars))

  # getting rid of baseflow for plotting because it's just 0 for the median lake
  vars <- vars[!grepl(pattern = 'baseflow', vars)]

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
    mutate(sw_in_wa = sw_in / Area_m2,
           baseflow_wa = baseflow / Area_m2,
           sw_in_la = sw_in / area,
           baseflow_la = baseflow / area,
           sw_out_la = sw_out / area,
           precip_la = precip / area,
           evap_la = evap / area,
           emit_la = emit / area,
           gpp = gpp * vol_epi,
           gpp_la = gpp / area,
           doc_resp_la = doc_resp / area,
           gw_in_la = gw_in / area,
           gw_out_la = gw_out / area,
           doc_loads_la = doc_loads / area,
           bury_la = bury / area,
           tp_loads_la = tp_loads / area) %>%
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

  # var_labels <- var_lookup$labs %>%
  #   bind_rows() %>%
  #   as.character()
  # names(site_labels) <- names(fig_config$site_abbrev)
  units <- var_lookup$units[vars] %>%
    bind_rows() %>% as.character()
  units <- paste(vars, '\n',units, sep ='')
  names(units) <- vars

  # n_plots = length(unique(monthly_ave$var))
  # n_ggs = trunc(n_plots / 9 + .99) # number of ggs needed assuming we're plotting 3x3

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
