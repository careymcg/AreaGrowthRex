#' Title
#'
#' @return
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @examples
plot_growth<-function(ssruns=MasterList,sslabels=Mlabels,savePlot = TRUE) {
  #This dataset was pulled from AKFIN but a query exists in newsbss repo under Get_EBS_Survey_Length_Age_and_Plot.R and is current as of 2021
  thedata<-read.csv(here::here("data","Survey_Age_Length.csv"))

  #check that data is for unique specimens
  thedata$ID<-paste0(thedata$SPECIMENID,"_",thedata$HAULJOIN)
  if (length(unique(thedata$ID))==nrow(thedata)) {
    print("unique IDs match number of rows")
  } else { print("unique IDs do not match number of rows")}

  thedata<-thedata %>% mutate(Length = LENGTH/10) %>% rename(Age=AGE,Sex = SEX) %>% filter(Sex!=3)
  thedata$Sex[thedata$Sex==1]<-"Male"
  thedata$Sex[thedata$Sex==2]<-"Female"

  thedata$GrowthMorph[thedata$GrowthMorph=="EASTERN"]<-"Eastern"
  thedata$GrowthMorph[thedata$GrowthMorph=="NOT_EASTERN"]<-"Western-Central"

  #jitter data
  thedata$Age<-thedata$Age + runif(n =nrow(thedata),min = 0, max = 0.25)
  thedata$Length<-thedata$Length + runif(n =nrow(thedata),min = 0, max = 0.25)


  ssgdata<-data.frame()
  for (i in 1:length(sslabels)) {
    ssgrowth<-data.frame(Model = sslabels[i],
                     Sex = ssruns[[i]]$endgrowth$"Sex",
                     GrowthMorph = ssruns[[i]]$endgrowth$"Bio_Pattern",
                     Age = ssruns[[i]]$endgrowth$"Age_Beg",
                     Length = ssruns[[i]]$endgrowth$"Len_Beg",
                     sd = ssruns[[i]]$endgrowth$"SD_Beg"*ssruns[[i]]$endgrowth$"Len_Beg")

    if (i==1) { ssgdata=ssgrowth}  else {
      ssgdata = rbind(ssgdata,ssgrowth)
    }
  }

  ssgdata<-ssgdata %>% mutate(lb = Length/exp(2*sqrt(log(1+sd^2)/Length^2)),
                              ub = Length*exp(2*sqrt(log(1+sd^2)/Length^2)))


  ssgdata$GrowthMorph[ssgdata$GrowthMorph==1]<-"Western-Central"
  ssgdata$GrowthMorph[ssgdata$GrowthMorph==2]<-"Eastern"

  ssgdata$Sex[ssgdata$Sex==1]<-"Female"
  ssgdata$Sex[ssgdata$Sex==2]<-"Male"


  #Plot total age vs length by sex for one area models, but color code data by growth morph
  p <- ggplot()
  p<-p + geom_point(data=thedata,aes(x=Age, y=Length,color = factor(GrowthMorph))) + scale_color_manual(values = alpha(c('#39568CFF','#55C667FF'),0.05)) +
     geom_line(data = ssgdata,aes(x = Age,y = Length,color = factor(GrowthMorph)),linewidth = 1.2,alpha = 1) +
     geom_ribbon(data = ssgdata,aes(x = Age,ymin = lb,ymax = ub,fill = factor(GrowthMorph)),alpha = 0.25, show.legend = FALSE) + scale_fill_manual(values=c('#39568CFF','#55C667FF')) +
     facet_grid(Sex~Model) + labs(y="Length (cm)",color = "Area")
  if (savePlot == TRUE) {
    ggsave(filename = file.path("doc",paste0("Plot_Growth_AllModels.png")),device = "png",width = 20,height = 10)
  }
  return(p)
}
