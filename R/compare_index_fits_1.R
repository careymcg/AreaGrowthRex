

#' Compare cpue indices and fits to indices for multiple runs
#'
#' @param ssruns
#' @param mlabels
#'
#' @return
#' @export
#'
#' @examples
compare_index_fits<-function(ssruns,mlabels) {
  index.t<-get_index_data(ssruns,mlabels)


  p<-ggplot(data=index.t, aes(x=year,y=obs),color='black') + geom_point(size=2) +
    geom_line(data=index.t, aes(x=year,y=exp,color=model)) +
    facet_wrap(.~fleet,ncol=1,scales='free_y')+
    geom_errorbar(data = index.t,aes(ymin=lower,ymax=upper),width=0.2,color='black',position=position_dodge(0.1)) +
    scale_color_viridis_d(direction = -1)  + labs(x='Year',y= 'Index value')
return(p)
}


