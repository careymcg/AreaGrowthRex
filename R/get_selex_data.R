#' Title
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
get_selex_data<-function(ssruns,mlabels) {
  selex.t<-tibble()
  for (i in 1:length(ssruns)) {
    selex<-ssruns[[i]]$"ageselex"
    ageselex<-selex %>% filter(Factor=="Asel" & Yr==2021) %>% select(-c(Label,Seas,Morph,Yr,Factor))
    stuff.t<-ageselex %>% pivot_longer(!c(Fleet,Sex),names_to="Age",values_to = "Proportion")
    stuff.t<-stuff.t %>% mutate(Model = mlabels[i])
    if (i==1) {
      selex.t<-stuff.t
    } else {
      selex.t<-rbind(selex.t,stuff.t)
    }
  }
return(selex.t)
}
