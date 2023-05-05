

#' Plot ssb estimates and uncertainty across model runs
#'
#' @param ssruns
#' @param mlabel
#'
#' @return
#' @export
#'
#' @examples
plot_ssbs<-function(ssruns,mlabel,showlegend) {

  ssb.t<-get_ssb_data(ssruns,mlabel)

  p<-ggplot(data = ssb.t,aes(x = as.numeric(Year),y=Value)) +
    geom_line(aes(color = factor(Model)),linewidth = 1.5,alpha = 1,show.legend = showlegend) + scale_fill_viridis_d() +
    geom_ribbon(aes(x = as.numeric(Year),ymin = lb,ymax = ub,fill = factor(Model)),alpha =0.2,show.legend = FALSE) + scale_fill_viridis_d() +
    labs(x="Year",y = "Spawning Biomass (t)",color = "Model")
  return(p)
}

# p <- ggplot()
# p<-p + geom_point(data=thedata,aes(x=Age, y=Length,color = factor(GrowthMorph))) + scale_color_manual(values = alpha(c('#39568CFF','#55C667FF'),0.05)) +
#   geom_line(data = ssgdata,aes(x = Age,y = Length,color = factor(GrowthMorph)),linewidth = 1.2,alpha = 1) +
#   geom_ribbon(data = ssgdata,aes(x = Age,ymin = lb,ymax = ub,fill = factor(GrowthMorph)),alpha = 0.25, show.legend = FALSE) + scale_fill_manual(values=c('#39568CFF','#55C667FF')) +
#   facet_grid(Sex~Model) + labs(y="Length (cm)",color = "Area")
