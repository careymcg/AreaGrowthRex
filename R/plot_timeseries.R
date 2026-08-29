

#' Plot time-series results from multiple Stock Synthesis 3 assessment models with uncertainty
#'
#' @param data.t time series data as a data frame or tibble with Year and Value columns
#' @param showlegend true or false
#' @param xlabel the label for the x axis
#' @param ylabel the label for the y axis
#' @param color_option used within scale_color_viridis_d for changing the color theme
#' @param color_direction switch when needed to show the data better
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 labeller
#' @importFrom ggplot2 theme
#' @importFrom viridis scale_color_viridis
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 coord_cartesian
#'
#' @examples
#' plot_timeseries(data.t = rec.t,xlabel= "Year",ylabel = "Age 0 (thousands)",showlegend = "none")
#'
plot_timeseries<-function(data.t,xlabel,ylabel,showlegend,color_option = "viridis",color_direction = 1) {

  tsplot<-ggplot2::ggplot(data = data.t,aes(x = as.numeric(Year),y=Value)) +
    geom_line(aes(color = factor(Model)),linewidth = 1.2,alpha = 1) +
    coord_cartesian(ylim = c(0, NA)) +
    geom_ribbon(aes(ymin = lb,ymax = ub,fill = factor(Model)),alpha =0.2,show.legend = FALSE) +
    labs(x=xlabel,y = ylabel,color = "Model") +
    scale_color_viridis_d(aesthetics = c("colour","fill"), option = color_option,direction = color_direction) +
    if (showlegend == "none") {
      guides(color = showlegend) } else {
      guides(col = guide_legend())
    }
  return(tsplot)
}
