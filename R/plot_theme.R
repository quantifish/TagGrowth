#' Plotting theme
#'
#' Plotting theme used by ggplot2
#'
#' @export
#' 
plot_theme <- function (base_size = 12, base_family = "") 
{
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(1.0)),
          axis.title=element_text(size=rel(1.2)),
          axis.ticks = element_line(colour = "black"), 
          strip.text = element_text(size = rel(1.2)),
          legend.key = element_rect(colour = "grey80"),
          legend.position = c(0.91, 0.15),
          panel.background = element_rect(fill = "white",colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2)
    )
}
