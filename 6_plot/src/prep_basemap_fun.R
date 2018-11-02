prep_basemap_fun <- function(focus_geoms_ind, secondary_geoms_ind = NULL, detail_geoms_ind = NULL,
                             waterbodies_ind = NULL, lakes_loc, polygon, scenarios, remake_file){

  if (!is.null(secondary_geoms_ind)){
    secondary_geoms <- readRDS(sc_retrieve(secondary_geoms_ind, remake_file))
  }

  if (!is.null(detail_geoms_ind)){
    detail_geoms <- readRDS(sc_retrieve(detail_geoms_ind, remake_file))
    # plot(detail_geoms, add = TRUE, lwd = 0.3, col = NA, border = 'grey95') # should style args be in a config?
  }
  if (!is.null(waterbodies_ind)){
    waterbodies <- readRDS(sc_retrieve(waterbodies_ind, remake_file))
    # plot(detail_geoms, add = TRUE, lwd = 0.3, col = NA, border = 'grey95') # should style args be in a config?
  }
  lake_id = readRDS(scenarios) %>%
    select(Permanent_) %>%
    dplyr::filter(!duplicated(Permanent_))

  lakes <- sf::st_read(lakes_loc)

  lakes = lakes %>%
    dplyr::filter(Permanent_ %in% lake_id$Permanent_)

  lakes_proj <- st_transform(lakes, crs = sf::st_crs(polygon))

  focus_geoms <- readRDS(sc_retrieve(focus_geoms_ind, remake_file))

  lims_orig = sf::st_bbox(focus_geoms)
  lims_adj = c(.3,3.1,.05,.86) # adjusting limits to zoom in on NHLD
  lims = lims_orig * lims_adj

  zoom = ggplot() +
    # geom_sf(data = secondary_geoms, fill = 'grey98') +
    geom_sf(data = focus_geoms, fill = 'grey98') +
    geom_sf(data = lakes_proj, fill = 'blue', color = 'blue', size = .05) +
    ylim(lims[c('ymin','ymax')]) +
    xlim(lims[c('xmin','xmax')]) +
    theme(panel.background = element_rect(size = 1, color ='black', fill= NA),
          panel.grid.major = element_line(color= 'transparent'),
          rect = element_blank(),
          plot.background = element_blank(),
          panel.ontop = T)
  # zoom


  context = ggplot() +
    geom_sf(data = secondary_geoms, fill = 'grey98') +
    geom_rect(aes(xmin = lims['xmin'], ymin = lims['ymin'], xmax = lims['xmax'], ymax = lims['ymax']), fill = NA, linetype ='solid',
              size = 1.5, color ='red') +
    ylim(lims_orig[c('ymin','ymax')]) +
    xlim(lims_orig[c('xmin','xmax')]) +
    theme(panel.background = element_rect(size = 1, color ='black', fill =NA),
          panel.grid.major = element_line(color= 'transparent'),
          rect = element_blank(),
          text = element_blank(),
          line = element_blank(),
          panel.ontop = T)

  # context

  g = ggdraw() +
    draw_plot(zoom, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(context, x= 0.088, y= .518, width = .35, height = .35)

  # g

  return(g)
}

