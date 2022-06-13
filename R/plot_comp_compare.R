
#' Plot comparison of fits to aggregated comp data (lengths or ages)
#'
#' @param ssruns summary of outputs from a set of Stock Synthesis runs generated from using r4ss functions SSgetoutput followed by SSsummarize
#' @param narea number of separate areas for plotting length distributions (makes separate plots by area for 2 area models)
#' @param mnames model names for plot legend labels
#' @param comptype plot age or length comps? "age" "length" are valid inputs
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
#' @importFrom ggplot2 labs
#'
#' @examples

plot_comp_compare <- function(ssruns, narea, mnames,comptype = "length") {
  # Make a big length data frame with all of the models included
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
  print("note this function currently only works for narea = 1")
  if (narea == 1) {
    one.t <- biglen1.t %>%
      filter(mname == "OneArea_NoFages" | mname == "OneArea_Fages") %>%
      select(Yr, Fleet, Sex, Bin, Obs, Exp, mname)
  }


  # aggregate over years
  one2.t <- one.t %>%
    group_by( Fleet,  Sex,  Bin,  mname) %>%
    summarise(SumObs = sum( Obs), SumExp = sum( Exp))
  thesum.t <- one2.t %>%
    group_by( Fleet,  Sex,  mname) %>%
    summarise(TotObs = sum( SumObs), TotExp = sum( SumExp))
  one3.t <- left_join(one2.t, thesum.t)
  one4.t <- one3.t %>% mutate(Obs =  SumObs /  TotObs, Exp =  SumExp /  TotExp)
  check <- one4.t %>%
    group_by( Fleet,  Sex,  mname) %>%
    summarise(checkobs = sum( Obs), checkexp = sum( Exp))
  one5.t <- one4.t %>% select( Fleet,  Sex,  Bin,  Obs,  Exp,  mname)

  onetry.t<-one5.t %>% filter(mname == "OneArea_NoFages")
  twotry.t<-one5.t %>% filter(mname == "OneArea_Fages")

  sex.labs <- c("Females", "Males")
  names(sex.labs) <- c("1", "2")
  fleet.labs <- c("Fishery", "Survey")
  names(fleet.labs) <- c("1", "2")
  lfits <- ggplot(one5.t) +
    geom_col(aes(x =  Bin, y =  Obs,color =  mname)) +
#    geom_bar(aes(x =  Bin, y =  Obs,color =  mname),stat='identity', alpha = 0.4) +
    geom_line(data = one5.t,aes(x = as.numeric( Bin), y =  Exp, color =  mname)) +
    facet_grid(Fleet ~ Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs)) +
    labs(x = "Bin", y = "Proportion")
  lfits

  #carey's experiment with one mname
  if (comptype == "length") {
 l1fits <- ggplot(onetry.t) +
    geom_bar(aes(x =Bin, y =  Obs),stat='identity', alpha = 0.4) +
    geom_line(data = onetry.t,aes(x = as.numeric( Bin), y =  Exp)) +
    facet_grid(Fleet ~ Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs)) +
    labs(x = "Bin", y = "Proportion")
  l1fits

    l2fits <- ggplot(twotry.t) +
      geom_bar(aes(x =Bin, y =  Obs),stat='identity', alpha = 0.4) +
      geom_line(data = twotry.t,aes(x = as.numeric( Bin), y =  Exp)) +
      facet_grid(Fleet ~ Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs)) +
      labs(x = "Bin", y = "Proportion")
    l2fits
  }


  #Jim's:
  # lfits <- one5.t %>% ggplot(aes(x=Bin,y=Obs,color=mname)) +
  #   geom_bar(stat='identity', alpha = 0.4) +
  #   geom_line(aes(y = Exp ),stat='identity') +
  #   facet_grid(Fleet~Sex, labeller = labeller(Sex = sex.labs, Fleet = fleet.labs)) +
  #   labs(x = "Length (cm)", y = "Proportion") +
  #   ggthemes::theme_few() ; lfits

  allplots<-list()
  allplots$combo<-lfits
  if (comptype == "length") {
    allplots$noages<-l1fits
    allplots$ages <-l2fits
  }
  return(allplots)
}
