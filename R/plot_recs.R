#' Title
#'
#' @param ssruns
#' @param mlabel
#'
#' @return
#' @export
#'
#' @examples
plot_recs<-function(ssruns,mlabel,showlegend) {

  rec.t<-get_rec_data(ssruns,mlabel)

  p<-ggplot(data = rec.t,aes(x = as.numeric(Year),y=Value)) +
    geom_line(aes(color = factor(Model)),linewidth = 1.5,alpha = 1,show.legend = showlegend) + scale_fill_viridis_d() +
    geom_ribbon(aes(x = as.numeric(Year),ymin = lb,ymax = ub,fill = factor(Model)),alpha =0.2,show.legend = FALSE) + scale_fill_viridis_d() +
    labs(x="Year",y = "Age 0 Recruitment (thousands)",color = "Model")
  return(p)
}
