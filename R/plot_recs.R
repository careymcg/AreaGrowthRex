#' Title
#'
#' @param ssruns
#' @param mlabel
#'
#' @return
#' @export
#'
#' @examples
plot_recs<-function(ssruns,mlabel,showlegend,showxlabel) {

  rec.t<-get_rec_data(ssruns,mlabel)
  if (showxlabel == TRUE) {
    p<-plot_timeseries(data.t = rec.t,xlabel = "Year",ylabel = "Age 0 (thousands)",showlegend = showlegend)
  } else {
    p<-plot_timeseries(data.t = rec.t,xlabel = "",ylabel = "Age 0 (thousands)",showlegend = showlegend)
  }

  # p<-ggplot(data = rec.t,aes(x = as.numeric(Year),y=Value)) +
  #   geom_line(aes(color = factor(Model)),linewidth = 1.5,alpha = 1,show.legend = showlegend) +
  #   geom_ribbon(aes(ymin = lb,ymax = ub,fill = factor(Model)),alpha =0.2,show.legend = FALSE) +
  #   labs(x="Year",y = "Age 0 (thousands)",color = "Model") +
  #   scale_color_viridis_d(aesthetics = c("colour","fill"))
  return(p)
}

# # specify aesthetic inside scale_color_viridis_d()
# p<-ggplot(data = ssb.t,aes(x = as.numeric(Year),y=Value)) +
#   geom_line(aes(color = factor(Model)),linewidth = 1.5,alpha = 1) +
#   geom_ribbon(aes(ymin = lb,ymax = ub,fill = factor(Model)),linetype = 2,alpha =0.2,show.legend = FALSE) +
#   labs(x="Year",y = "Spawning Biomass (t)",color = "Model") +
#   scale_color_viridis_d(aesthetics = c("colour","fill"))
