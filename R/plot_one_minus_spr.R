

#' Plot time series of fishing intensity in terms of one minus SPR
#'
#' @param ssruns
#' @param mlabel
#' @param showlegend
#' @param showxlabel
#'
#' @return
#' @export
#'
#' @examples
#' fishing_intensity_plot<-plot_one_minus_spr(ssruns = MasterList,mlabel = Mlabels,showxlabel = TRUE,showlegend = "yes")
plot_one_minus_spr<-function(ssruns,mlabel,showlegend,showxlabel,color_option = "viridis",color_direction = "color_direction") {

  spr.t<-get_timeseries_data(ssruns,mlabel,label_type = "SPRratio_")

  if (showxlabel == TRUE) {
    p<-plot_timeseries(data.t = spr.t,xlabel = "Year",ylabel = "1-SPR",showlegend = showlegend,color_option = color_option,color_direction= color_direction)
  } else {
    p<-plot_timeseries(data.t = spr.t,xlabel = "",ylabel = "1-SPR",showlegend = showlegend,color_option = color_option,color_direction = color_direction)
  }

  return(p)
}
