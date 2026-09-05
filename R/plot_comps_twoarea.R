
#' Plot comparison of fits to aggregated comp data (lengths or ages) for one area models
#'
#' @param ssruns summary of outputs from a set of Stock Synthesis runs generated from using r4ss functions SSgetoutput followed by SSsummarize
#' @param narea number of separate areas for plotting length distributions (makes separate plots by area for 2 area models)
#' @param mnames model names for plot legend labels
#' @param comptype plot age or length comps? "age" "length" are valid inputs
#' @param saveplots save each plot as a .png file
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 labeller
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 scale_color_manual
#' @importFrom viridis scale_color_viridis
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 ggtitle
#'
#' @examples

plot_comps_twoarea <- function(ssruns, narea, mnames,comptype = "length", saveplots = TRUE) {
  # Make a big length data frame with all of the models included
  if (comptype == "length") {
    binlabel<-"Lengths (cm)" }
  else { binlabel <-"Ages"}

  nmodel <- length(ssruns)
  biglen.df <- data.frame()

  if (comptype == "length") {
    biglen.t<-get_lengths(ssruns,mnames)
  } else { #if comptype =="ages"
    biglen.t<-get_ages(ssruns,mnames)
  }

  # multiply observations and expectations by the adjusted sample size for each year and area and sex and fleet (put everything in terms of numbers)
  biglen1.t<-biglen.t %>% mutate(Obs=Obs*Nsamp_adj,Exp = Exp*Nsamp_adj)
  # only the one-area model runs


  one.t <- biglen1.t %>%
    select(Yr, Fleet, Sex, Bin, Obs, Exp, mname, Area)


  # aggregate over years
  one2.t <- one.t %>%
    group_by( Fleet,  Sex,  Bin,  mname, Area) %>%
    summarise(SumObs = sum( Obs), SumExp = sum( Exp))
  thesum.t <- one2.t %>%
    group_by( Fleet,  Sex,  mname, Area) %>%
    summarise(TotObs = sum( SumObs), TotExp = sum( SumExp))
  one3.t <- left_join(one2.t, thesum.t)
  one4.t <- one3.t %>% mutate(Obs =  SumObs /  TotObs, Exp =  SumExp /  TotExp)
  check <- one4.t %>%
    group_by( Fleet,  Sex,  mname, Area) %>%
    summarise(checkobs = sum( Obs), checkexp = sum( Exp))
  one5.t <- one4.t %>% select( Fleet,  Sex,  Bin,  Obs,  Exp,  mname, Area)


  #    onetry.t<-one5.t %>% filter(mname == mnames[1])
  #    twotry.t<-one5.t %>% filter(mname == mnames[2])

  #replace above with:
  try_list<-list()
  for (imodel in 1:length(mnames)) {
    try_list[[imodel]]<-one5.t %>% filter(mname==mnames[imodel])
  }

  sex.labs <- c("Females", "Males")
  names(sex.labs) <- c("1", "2")
  fleet.labs <- c("Fishery", "Survey")
  names(fleet.labs) <- c("1", "2")
  if (narea>1) {
    area.labs<-c("Western-Central","Eastern")
    names(area.labs)<-c("1","2")
  } else {
    area.labs<-c("All GOA")
    names(area.labs)<-"1"
  }

  #Outdated:
  #   lfits <- ggplot(one5.t) +
  #     geom_col(aes(x =  Bin, y =  Obs,color = mname), position = "identity", alpha = 1) + scale_color_viridis(discrete = TRUE) +
  # #    geom_bar(aes(x =  Bin, y =  Obs,color =  mname),stat='identity', alpha = 0.4) +
  #     geom_line(data = one5.t,aes(x = as.numeric( Bin), y =  Exp, color =  mname)) +
  #     facet_grid(Fleet ~ Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs)) +
  #     labs(x = "Bin", y = "Proportion")
  #   lfits


  #combo: one mname at a time but on the same plots
  trylist_2<-list()
  for (iarea in 1:narea) {
    if (narea == 2 & iarea==2 & comptype == "age") {
      print("no age compositions for the Eastern GOA")
    } else {
      one5a.t<-one5.t %>% filter(Area==iarea)
      #onetrya.t<-onetry.t %>% filter(Area==iarea)
      #twotrya.t<-twotry.t %>% filter(Area==iarea)
      for (imodel in 1:length(mnames)) {
        trylist_2[[imodel]]<-try_list[[imodel]] %>% filter(Area==iarea)
      }
      lcombo <- ggplot(one5a.t)
      for (imodel in 1:length(mnames)) {
        lcombo<-lcombo + geom_col(data = trylist_2[[imodel]],aes(x =  Bin, y =  Obs), fill = "#55C667FF", alpha = 0.6)
      }    #    geom_bar(aes(x =  Bin, y =  Obs,color =  mname),stat='identity', alpha = 0.4) +
      lcombo<-lcombo + geom_line(data = one5a.t,aes(x = as.numeric( Bin), y =  Exp, color =  mname),linewidth = 1.2) +
        scale_fill_brewer(palette = "Accent") +
        labs(x = binlabel, y = "Proportion",color = "Model")
      if (iarea==2 & Fleet == 1) {
        lcombo<-lcombo + facet_grid(~ Sex, labeller = labeller(Sex = sex.labs))
        print ("Reminder: no fishery data in the Eastern GOA")
      } else {
        lcombo<-lcombo + facet_grid(Fleet ~ Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs))
      }

      if (narea>1) {
        lcombo<-lcombo + ggtitle(area.labs[iarea])
      }
      lcombo
      ggsave(filename = file.path("doc",paste0("Comps_",comptype,"Area",area.labs[iarea],".png")),device = "png",width = 10,height = 5)
    }
  }

  #one mname at a time (for debugging, but not included in the MS)
  if (comptype == "length") {
    l1fits <- ggplot(trylist_2[[1]]) +
      geom_bar(aes(x =Bin, y =  Obs),stat='identity', alpha = 0.4) +
      geom_line(data = trylist_2[[1]],aes(x = as.numeric( Bin), y =  Exp),linewidth = 1.2) +
      facet_grid(Fleet ~ Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs)) +
      labs(x = binlabel, y = "Proportion")
    l1fits
    ggsave(filename = file.path("doc",paste0("Aux_OneAreaNoFages_",comptype,".png")),device = "png")

    l2fits <- ggplot(trylist_2[[2]]) +
      geom_bar(aes(x =Bin, y =  Obs),stat='identity', alpha = 0.4) +
      geom_line(data = trylist_2[[2]],aes(x = as.numeric( Bin), y =  Exp),linewidth = 1.2) +
      facet_grid(Fleet ~ Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs)) +
      labs(x = binlabel, y = "Proportion")
    l2fits
    #ggsave(filename = file.path("doc",paste0("Aux_OneAreaFages_",comptype,".png")),device = "png")

  }


  #Jim's:
  # lfits <- one5.t %>% ggplot(aes(x=Bin,y=Obs,color=mname)) +
  #   geom_bar(stat='identity', alpha = 0.4) +
  #   geom_line(aes(y = Exp ),stat='identity') +
  #   facet_grid(Fleet~Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs)) +
  #   labs(x = binlabel, y = "Proportion") +
  #   ggthemes::theme_few() ; lfits

  allplots<-list()
  allplots$combo<-lcombo
  if (comptype == "length") {
    #allplots$combo<-lcombo
    allplots$noages<-l1fits
    allplots$ages <-l2fits
  }
  return(allplots)
}
