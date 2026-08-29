#' Compare index fits across models
#'
#' @param ssruns a list from r4ss with output from one or more runs
#' @param mlabels labels for each of the runs included in ssruns list
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 scale_color_viridis_d
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_line
#' @examples
#' compare_index_fits(ssruns = ssgetoutputlist,mlabels = c("Model 25.1", "Model 25.2"))
compare_index_fits<-function(ssruns,mlabels) {
  index.t<-get_index_data(ssruns,mlabels)


  p<-ggplot(data=index.t, aes(x=year,y=obs),color='black') + geom_point(size=2) +
    geom_line(data=index.t, aes(x=year,y=exp,color=model)) +
    coord_cartesian(ylim = c(0, NA)) +
    facet_wrap(.~fleet,ncol=1,scales='free_y')+
    geom_errorbar(data = index.t,aes(ymin=lower,ymax=upper),width=0.2,color='black',position=position_dodge(0.1)) +
    scale_color_viridis_d(direction = -1)  + labs(x='Year',y= 'Index value')
  return(p)
}


