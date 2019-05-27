pub_theme <- function(){
  theme_bw() +
    theme(text = element_text(size = 8),
          title = element_text(size = 8),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 7, face = "bold", margin = margin(b = 0)),
          plot.subtitle = element_text(size = 7),
          plot.tag = element_text(size = 7),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_text(hjust = 0.5),
          legend.key.size = unit(3, "mm"),
          legend.margin = margin(),
          axis.ticks.x = element_line(colour = "grey30", size = 0.05),
          axis.ticks.y = element_line(colour = "grey30", size = 0.25),
          # panel.border = element_blank(),
          # axis.line.y = element_line(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank())
}

width <- function(type, unit=NULL){
  # Ecology letters has a paper width of 200 mm
  widths <- dplyr::case_when(
    type == "single" ~ 3.23,
    type == "double" ~ 6.81,
    TRUE ~ 4.33
  )
  if(is.null(unit)) return(widths)
  else if (unit=="in") {
    widths %>%
      as.character() %>%
      paste0('in')
  }
}

remove_legend <- function(x){
  require(ggplot2)
  x + theme(legend.position = 'none')
}

common_graphic_metrics <- function(){
  list(
    pal_rb3 = RColorBrewer::brewer.pal(4, "Greys")[c(3,1,2)],
    pal_el_green = colorRampPalette(c("#d9e6cb", "#9fc17c", "#0d1108"), bias = 1, space = "rgb",
                                    interpolate = "linear", alpha = FALSE)(9),
    size_errorbars =  0.20,
    color_errorbars = "grey30",
    color_errorbars_light = "grey75",
    log1p_axis_breaks_10 = c(0, 10, 100, 1000, 10000),
    point_size = 1,
    color_references = "grey50",
    size_references = 0.25,
    fill_rows = "grey95"
  )
}

cgm <- function(){
  common_graphic_metrics()
}
