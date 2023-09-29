#' Pull maturity estimates from the ss models and aggregate
#'
#' @param ssruns
#' @param mlabels
#'
#' @return
#' @export
#'
#'@import dplyr
#'@importFrom tidyr pivot_longer
#' @examples
get_maturity_data<-function(ssruns,mlabels) {
  #maturity shouldn't be different for the different models so something isn't right.
  #Better to get the maturity curve from the input file and apply it.
  maturity.t<-tibble()
  for (i in 1:length(ssruns)) {
   selex<-ssruns[[i]]$"ageselex"
    maturity<-selex %>% filter(Factor=="Fecund" & Yr==2021 & Sex==1) %>% select(-c(Label,Seas,Yr,Factor))
    bodywt<-selex %>% filter(Factor =="bodywt" & Yr == 2021 & Sex==1) %>% select(-c(Label,Seas,Yr,Factor))
    mstuff.t<-maturity %>% pivot_longer(!c(Sex,Fleet,Morph),names_to="Age",values_to = "fecundity") %>% select(-Fleet)
    bstuff.t<- bodywt %>% pivot_longer(!c(Sex,Fleet,Morph),names_to="Age",values_to = "bodywt")
    mstuff2.t<-full_join(mstuff.t,bstuff.t)
    mstuff2.t<-mstuff2.t %>% mutate(Model = mlabels[i],maturity = fecundity/bodywt)
    if (i==1) {
      maturity.t<-mstuff2.t
    } else {
      maturity.t<-rbind(maturity.t,mstuff2.t)
    }
  }
  return(maturity.t)
}
