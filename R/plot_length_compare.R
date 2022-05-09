
#' Plot comparison of fits to aggregated length data
#'
#' @param ssruns summary of outputs from a set of Stock Synthesis runs generated from using r4ss functions SSgetoutput followed by SSsummarize
#' @param narea number of separate areas for plotting length distributions (makes separate plots by area for 2 area models)
#' @param mnames model names for plot legend labels and
#' @return
#' @export
#' @importFrom rlang .data
#'
#' @examples
plot_length_compare<-function(ssruns,narea,mnames)
{
   #Make a big length data frame with all of the models included
   nmodel<-length(ssruns)
   biglen.df<-data.frame()
   for (imodel in 1:nmodel) {
     ssruns[[imodel]]$lendbase$mname<-mnames[imodel] #make a variable out of the model name
     if (imodel==1) {
       biglen.df<-ssruns[[1]]$lendbase
     }
     if (imodel>1) {
       biglen.df<-rbind(biglen.df,ssruns[[imodel]]$lendbase)
     }
    }

   #Filter by number of areas (different plots for 1 vs 2 area models)
   biglen.t<-tibble::as_tibble(biglen.df)
   #only the one-area model runs
   print("note this function currently only works for narea = 1")
    if (narea == 1) {
     one.t<-biglen.t%>%dplyr::filter(.data$mname=="OneArea_NoFages" | .data$mname == "OneArea_Fages") %>% dplyr::select(.data$Yr,.data$Fleet,.data$Sex,.data$Bin,.data$Obs,.data$Exp,.data$mname)
   }
   # one.t<-biglen.t%>%filter(mname=="OneArea_NoFages") %>% select(Yr,Fleet,Sex,Bin,Obs,Exp,mname)
  #aggregate over years
   one2.t<-one.t %>% dplyr::group_by(.data$Fleet,.data$Sex,.data$Bin,.data$mname) %>% dplyr::summarise(SumObs = sum(.data$Obs),SumExp = sum(.data$Exp))
   thesum.t<-one2.t %>% dplyr::group_by(.data$Fleet,.data$Sex,.data$mname) %>% dplyr::summarise(TotObs = sum(.data$SumObs),TotExp=sum(.data$SumExp))
   one3.t<-dplyr::left_join(one2.t,thesum.t)
   one4.t<-one3.t %>% dplyr::mutate(Obs=.data$SumObs/.data$TotObs,Exp=.data$SumExp/.data$TotExp)
   check<-one4.t %>% dplyr::group_by(.data$Fleet,.data$Sex,.data$mname) %>% dplyr::summarise(checkobs=sum(.data$Obs),checkexp=sum(.data$Exp))
   one5.t<-one4.t %>% dplyr::select(.data$Fleet,.data$Sex,.data$Bin,.data$Obs,.data$Exp,.data$mname)

   sex.labs <- c("Females","Males")
   names(sex.labs) <-c("1","2")
   fleet.labs<- c("Fishery","Survey")
   names(fleet.labs)<-c("1","2")
  lfits<-ggplot2::ggplot(one5.t) +
            ggplot2::geom_polygon(ggplot2::aes(x=.data$Bin,y=.data$Obs), fill = "lightblue",alpha = 0.4) +
            ggplot2::geom_line(ggplot2::aes(x=.data$Bin,y=.data$Exp,color = .data$mname)) +
            ggplot2::facet_grid(Fleet~Sex,labeller = ggplot2::labeller(Sex = sex.labs,Fleet=fleet.labs)) +
            ggplot2::labs(x = "Length (cm)",y = "Proportion")
   lfits

  }
