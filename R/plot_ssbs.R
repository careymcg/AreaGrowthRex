

#' Plot ssb estimates and uncertainty across model runs
#'
#' @param ssruns
#' @param mlabel
#'
#' @return
#' @export
#'
#' @examples
plot_ssbs<-function(ssruns,mlabel,showlegend,showxlabel) {

  ssb.t<-get_ssb_data(ssruns,mlabel)
  if (showxlabel == TRUE) {
    p<-plot_timeseries(data.t = ssb.t,xlabel = "Year",ylabel = "Spawning Biomass (t)",showlegend = showlegend)
  } else {
    p<-plot_timeseries(data.t = ssb.t,xlabel = "",ylabel = "Spawning Biomass (t)",showlegend = showlegend)
  }

# # specify aesthetic inside scale_color_viridis_d()
#   p<-ggplot(data = ssb.t,aes(x = as.numeric(Year),y=Value)) +
#      geom_line(aes(color = factor(Model)),linewidth = 1.5,alpha = 1) +
#      geom_ribbon(aes(ymin = lb,ymax = ub,fill = factor(Model)),linetype = 2,alpha =0.2,show.legend = FALSE) +
#      labs(x="Year",y = "Spawning Biomass (t)",color = "Model") +
#       scale_color_viridis_d(aesthetics = c("colour","fill"))

  return(p)
}

