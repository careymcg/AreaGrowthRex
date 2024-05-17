
#' Get the ssb output from ss models and aggregate
#'
#' @param ssruns
#'
#' @return
#' @export
#'
#' @importFrom tidyr separate
#' @importFrom stringr str_detect
#' @import dplyr
#' @examples
get_ssb_data<-function(ssruns,mlabel) {
  alldata<-list()
  ssb<-list()
  ssb.df<-data.frame()
  for (i in 1:length(ssruns)) {
    alldata[[i]]<-ssruns[[i]]$derived_quants

    ssb[[i]]<-alldata[[i]] %>% filter(str_detect(Label,"SSB_")) %>%filter(Label!="B_MSY/SSB_unfished") %>%
      separate(Label,c("Variable","Year"),sep = "_",remove = FALSE)

    #ssb[[i]]<-alldata[[i]] %>% filter(str_detect(Label,"SSB_")) %>%
    #  separate(Label,c("Variable","Year"),sep = "_",remove = FALSE)
    ssb[[i]]$Model<-mlabel[i]
   if (i == 1) {
     ssb.df<-ssb[[i]]
    } else {
     ssb.df<-rbind(ssb.df,ssb[[i]])
    }
  }
  ssb.t<-tibble(ssb.df) %>%
    select(c(Label,Variable,Year,Value,StdDev,Model)) %>%
    filter(str_detect(Year,"^\\s*[0-9]*\\s*$")) %>%
    select(Model,Year,Variable,Value,StdDev) %>%
    mutate(lb = Value/exp(2*sqrt(log(1+StdDev^2/Value^2))),
                          ub = Value*exp(2*sqrt(log(1+StdDev^2/Value^2))))

  #double lb=value(SSB(i)/exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
  #double ub=value(SSB(i)*exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
  return(ssb.t)
}

