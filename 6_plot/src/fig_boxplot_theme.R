

theme_boxplot <- ggplot2::theme(legend.title = element_blank(),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank(),
                                axis.text = element_text(size = 15),
                                strip.text = element_text(size = 15),
                                axis.title = element_text(size = 15),
                                axis.line = element_line(colour = "black"),
                                legend.position = c(.15,.90),
                                legend.key = element_blank(),
                                strip.background = element_blank(),
                                plot.margin = unit(c(1,3,1,1),'lines'))
