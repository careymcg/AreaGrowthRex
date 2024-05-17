#' Grab catch data for each model and make a tibble including all models
#'
#' @param ssruns
#' @param mlabel
#'
#' @return
#' @export
#'
#' @examples
#' get_catch_data(ssruns = ssruns,mlabel = mlabel)
get_catch_data<-function(ssruns,mlabel) {
  alldata<-list()
  catchdata<-list()
  catchdata.df<-data.frame()
  for (i in 1:length(ssruns)) {
    catchdata[[i]]<-ssruns[[i]]$catch

    catchdata[[i]]$Model<-mlabel[i]
    if (i == 1) {
      catchdata.df<-catchdata[[i]]
    } else {
      catchdata.df<-rbind(catchdata.df,catchdata[[i]])
    }
  }
  catchdata.t<-tibble(catchdata.df) %>%
    select(c(Fleet_Name,Area,Yr,Obs,Model)) #could keep more variables here if needed
  return(catchdata.t)
}

