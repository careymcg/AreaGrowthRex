#' Get recruitment data for each run into one tibble
#'
#' @param ssruns
#' @param mlabel
#'
#' @return
#' @export
#'
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#' @import dplyr
#' @examples
get_rec_data<-function(ssruns,mlabel) {
  alldata<-list()
  rec<-list()
  rec.df<-data.frame()
  for (i in 1:length(ssruns)) {
    alldata[[i]]<-ssruns[[i]]$derived_quants
    rec[[i]]<-alldata[[i]] %>% filter(str_detect(Label,"Recr_")) %>%
      separate(Label,c("Variable","Year"),sep = "_",remove = FALSE)
    rec[[i]]$Model<-mlabel[i]
    if (i == 1) {
      rec.df<-rec[[i]]
    } else {
      rec.df<-rbind(rec.df,rec[[i]])
    }
  }
  rec.t<-tibble(rec.df) %>%
    select(c(Label,Variable,Year,Value,StdDev,Model)) %>%
    filter(str_detect(Year,"^\\s*[0-9]*\\s*$")) %>%
    select(Model,Year,Variable,Value,StdDev) %>%
    mutate(lb = Value/exp(2*sqrt(log(1+StdDev^2/Value^2))),
           ub = Value*exp(2*sqrt(log(1+StdDev^2/Value^2))))
  #data_new <- data[!is.na(as.numeric(data$x1)), ]


  # ssb.t<-tibble(ssb.df) %>%
  #   select(c(Label,Variable,Year,Value,StdDev,Model)) %>%
  #   filter(str_detect(Year,"^\\s*[0-9]*\\s*$")) %>%
  #   select(Model,Year,Variable,Value,StdDev) %>%
  #   mutate(lb = Value/exp(2*sqrt(log(1+StdDev^2/Value^2))),
  #          ub = Value*exp(2*sqrt(log(1+StdDev^2/Value^2))))


  #double lb=value(rec(i)/exp(2.*sqrt(log(1+square(rec.sd(i))/square(rec(i))))));
  #double ub=value(rec(i)*exp(2.*sqrt(log(1+square(rec.sd(i))/square(rec(i))))));
  return(rec.t)
}
