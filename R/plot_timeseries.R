

#' Plot time-series results from multiple Stock Synthesis 3 assessment models with uncertainty
#'
#' @param data.t
#' @param showlegend
#'
#' @return
#' @export
#'
#' @examples
#' plot_timeseries(data.t = rec.t,xlabel= "Year",ylabel = "Age 0 (thousands)",showlegend = "none")
#'
plot_timeseries<-function(data.t,xlabel,ylabel,showlegend) {

  tsplot<-ggplot(data = data.t,aes(x = as.numeric(Year),y=Value)) +
    geom_line(aes(color = factor(Model)),linewidth = 1.5,alpha = 1) +
    geom_ribbon(aes(ymin = lb,ymax = ub,fill = factor(Model)),alpha =0.2,show.legend = FALSE) +
    labs(x=xlabel,y = ylabel,color = "Model") +
    scale_color_viridis_d(aesthetics = c("colour","fill")) +
    if (showlegend == "none") {
      guides(color = showlegend) } else {
      guides(col = guide_legend())
    }
  return(tsplot)
}
