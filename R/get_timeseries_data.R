#' Grab and format any derived quantity timeseries for all models
#'
#' @param ssruns
#' @param mlabel
#' @param label_type
#'
#' @return
#' @export
#'
#' @examples
#'  ssb.t<-get_timeseries_data(ssruns,mlabel,label_type = "SSB_")
#'  rec.t<-get_timeseries_data(ssruns,mlabel,label_type = "Recr_")
#'  spr.t<-get_timeseries_data(ssruns,mlabel,label_type = "SPRratio_")
#'
get_timeseries_data<-function(ssruns,mlabel,label_type) {
  alldata<-list()
  ts<-list()
  ts.df<-data.frame()
  for (i in 1:length(ssruns)) {
    alldata[[i]]<-ssruns[[i]]$derived_quants

    ts[[i]]<-alldata[[i]] %>% filter(str_detect(Label,label_type)) %>%
      filter(Label!="B_MSY/SSB_unfished") %>%
      separate(Label,c("Variable","Year"),sep = "_",remove = FALSE)

    #ts[[i]]<-alldata[[i]] %>% filter(str_detect(Label,"ts_")) %>%
    #  separate(Label,c("Variable","Year"),sep = "_",remove = FALSE)
    ts[[i]]$Model<-mlabel[i]
    if (i == 1) {
      ts.df<-ts[[i]]
    } else {
      ts.df<-rbind(ts.df,ts[[i]])
    }
  }
  ts.t<-tibble(ts.df) %>%
    select(c(Label,Variable,Year,Value,StdDev,Model)) %>%
    filter(str_detect(Year,"^\\s*[0-9]*\\s*$")) %>%
    select(Model,Year,Variable,Value,StdDev) %>%
    mutate(lb = Value/exp(2*sqrt(log(1+StdDev^2/Value^2))),
           ub = Value*exp(2*sqrt(log(1+StdDev^2/Value^2))))
  return(ts.t)

}
