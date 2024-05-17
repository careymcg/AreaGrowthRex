#' Plot total catches. Easily extendable to plotting by areas included in a model.
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
plot_total_catch<-function(ssruns,mlabel,showlegend,showxlabel) {

  #rec.t<-get_rec_data(ssruns,mlabel)
  catch.t<-get_catch_data(ssruns = ssruns,mlabel = mlabel)
  #just plotting total catches here (can be by area if desired - change the next line accordingly),so don't need all the models unless total catches were different.
  FirstModel<-catch.t$Model[1]
  catch.t<-catch.t %>% filter(Model==FirstModel)

  if (showxlabel == TRUE) {
    p<-ggplot(data = catch.t,aes(x = as.numeric(Yr),y=Obs)) +
      geom_line(aes(color = factor(Model)),linewidth = 1.5,alpha = 1,show.legend = showlegend) +
      labs(x="Year",y = "Total Catch (t)",color = "Area") +
      scale_color_viridis_d()  } else {

    p<-ggplot(data = catch.t,aes(x = as.numeric(Yr),y=Obs)) +
      geom_line(aes(color = factor(Model)),linewidth = 1.5,alpha = 1,show.legend = showlegend) +
      labs(x="",y = "Total Catch (t)",color = "Area") +
      scale_color_viridis_d()   }


  return(p)
}
