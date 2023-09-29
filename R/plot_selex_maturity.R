#' Title
#'
#' @param ssruns
#' @param sel_data
#'
#' @return
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom ggpubr ggarrange
#'
#' @examples
plot_selex_maturity<-function(ssruns,mlabels) {
  sel_data<-get_selex_data(ssruns=ssruns,mlabels=mlabels)
  #get maturity-at-age vector
  #this should change in the future if more than one maturity curve is specified in a model or across models
  param.est<-ssruns[[1]]$parameters
  mat.50<-param.est %>% filter(Label == "Mat50%_Fem_GP_1") %>% select(Value)
  mat.slope<-param.est %>% filter(Label=="Mat_slope_Fem_GP_1") %>% select(Value)
  age<-seq(from = 0,to=20,by=1)
  prop<- 1/(1+exp(as.numeric(mat.slope)*(age-as.numeric(mat.50))))
  mat.t<-tibble(age = age,prop = prop)

  sel_data.f<-sel_data %>% filter(Sex==1)
  sel_data.m<-sel_data %>% filter(Sex == 2)

  sex.labs <- c("Females", "Males")
  names(sex.labs) <- c("1", "2")
  fleet.labs <- c("Fishery", "Survey (All or Western-Central)","Survey (Eastern)")
  names(fleet.labs) <- c("1", "2","3")

  p<-ggplot() +
    geom_line(data = mat.t,aes(x=age,y=prop),color = "black",linewidth = 1.3,linetype = "dashed")  +
    geom_line(data = sel_data.f,aes(x = as.numeric(Age),y=as.numeric(Proportion),color = as.factor(Model)),linewidth = 1.3) +
    scale_color_viridis_d(direction = -1) +
    labs(x = "Age",y = "Proportion",color = "Model") +
    ggtitle("Female Selectivity and Maturity") +
    facet_wrap(~Fleet,labeller = labeller(Fleet = fleet.labs))
p
p2<-ggplot() +
  geom_line(data = sel_data.m,aes(x = as.numeric(Age),y=as.numeric(Proportion),color = as.factor(Model)),linewidth = 1.3) +
  scale_color_viridis_d(direction =-1) +
  labs(x = "Age",y="Proportion",color = "Model") +
  ggtitle("Male Selectivity") +
  facet_wrap(~Fleet,labeller = labeller(Fleet = fleet.labs))
p3<-ggarrange(p,p2,nrow = 2,common.legend = T,legend = "bottom")

return(p3)
}
