prep_basemap_hydro <- function(focus_geoms_ind, secondary_geoms_ind = NULL, detail_geoms_ind = NULL,
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

  ## pull out only Trout Lake and Crystal and have that be a bounding box.
  ## Create new plot with those lims and label only Trout and crystal lake
  lakes_proj = mutate(lakes_proj, Permanent_ = as.character(Permanent_))

  focus_lakes = dplyr::filter(lakes_proj, Permanent_ %in% c('69886228','69886510'))

  trout_lims_orig = sf::st_bbox(focus_lakes)
  trout_lims = trout_lims_orig
  trout_lims[4] = 1621100

  lims_orig = sf::st_bbox(focus_geoms)
  lims_adj = c(.2,3.15,.05,.86) # adjusting limits to zoom in on NHLD
  lims = lims_orig * lims_adj

  zoom = ggplot() +
    # geom_sf(data = secondary_geoms, fill = 'grey98') +
    geom_sf(data = focus_geoms, fill = 'grey98') +
    geom_sf(data = lakes_proj, fill = 'dodgerblue', color = 'dodgerblue', size = .05) +
    geom_rect(aes(xmin = trout_lims['xmin'], ymin = trout_lims['ymin'],
                  xmax = trout_lims['xmax'], ymax = trout_lims['ymax']), fill = NA, linetype ='solid',
              size = 1.2, color ='red') +
    ylim(lims[c('ymin','ymax')]) +
    xlim(lims[c('xmin','xmax')]) +
    theme(panel.background = element_rect(size = 1, color ='black', fill= NA),
          panel.grid.major = element_line(color= 'transparent'),
          rect = element_blank(),
          plot.background = element_blank(),
          panel.ontop = T,
          axis.title = element_blank(),
          axis.text = element_text(size = 24)) +
    annotate('text', x = c(-116015,-12000), y = c(1570000,1640000),
             label = c('Wisconsin','Michigan'), size = 10, color = 'grey50')

   zoom

  context = ggplot() +
    geom_sf(data = secondary_geoms, fill = 'grey98') +
    geom_rect(aes(xmin = lims['xmin'], ymin = lims['ymin'],
                  xmax = lims['xmax'], ymax = lims['ymax']), fill = NA, linetype ='solid',
              size = 1.5, color ='blue') +
    ylim(lims_orig[c('ymin','ymax')]) +
    xlim(lims_orig[c('xmin','xmax')]) +
    theme(panel.background = element_rect(size = 1, color ='black', fill =NA),
          panel.grid.major = element_line(color= 'transparent'),
          rect = element_blank(),
          text = element_blank(),
          line = element_blank(),
          panel.ontop = T)

   context

  trout_zoom = ggplot() +
    geom_sf(data = lakes_proj, fill = 'lightblue', color = 'lightblue', alpha = .8) +
    geom_sf(data = focus_lakes, fill = 'dodgerblue', color = 'blue') +
    ylim(trout_lims[c('ymin','ymax')]) +
    xlim(trout_lims[c('xmin','xmax')]) +
    theme(panel.background = element_rect(size = 1,
                                          color ='black',
                                          fill= 'white'),
          rect = element_blank(),
          plot.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 12)) +
    annotate('text', x = c(-77500,-73200), y = c(1616000,1612200),
             label = c('TR','CR'), size = 6, color = 'white')

  trout_zoom

  g = ggdraw() +
    draw_plot(zoom, x = 0, y = 0, width = 1, height = 1) +
    draw_plot(context, x= 0.104, y= .65, width = .35, height = .35) +
    draw_plot(trout_zoom, x = .4939, y = .499, width = .5, height = .5)

  g

  return(g)
}

