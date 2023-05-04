
#' Write projection data file for 2 area models each with a growth morph using AFSC projection modeling code
#'
#' @param data_file
#' @param data
#' @param GrowthMorph
#' @param area
#' @param Narea
#' @param Nmorph
#' @param NAGES
#' @param FY
#' @param LY
#' @param RecAge
#' @param species
#'
#' @return
#' @export
#'
#' @examples
write_proj_ss_2Area<-function(data_file="ForProjections.dat",data=replist,GrowthMorph = 1,area = 1,Narea = 2,Nmorph = 2,NAGES=30,FY=1978,LY=2017,RecAge = 1,species){
  #Written by Steve Barbeaux and Carey McGilliard expanded
  #2013
  #Example:
  #MyOutput =
  #write_proj(data_file="Flathead_Proj_2.dat",data=MyOutput,NAGES=27,FY=1978,LY=2013,RecAge = 3)
  if (Nmorph == 2) {
    Morph1_F<-1
    Morph2_F<-2
    Morph1_M<-3
    Morph2_M<-4
  }
  ## writing prjection file
  ## mean 5 Yr F
  Y5<-LY-5
  M1<-as.numeric(subset(data$M_at_age,Yr==(LY-1)&Sex==1&Bio_Pattern==GrowthMorph)[,(4+RecAge-1):(NAGES+3)]) #this is weird bc rick has M for plus gp = NA
  M2<-as.numeric(subset(data$M_at_age,Yr==(LY-1)&Sex==2&Bio_Pattern==GrowthMorph)[,(4+RecAge-1):(NAGES+3)])
  F_5<-mean( data$timeseries$"F:_1"[data$timeseries$Yr>Y5 & data$timeseries$Area==area])
  ## population weight at age for females
  #Steve's trick: fecundity*maturity: WGT_F<-subset(data$endgrowth,data$endgrowth$Sex==1)[2:(NAGES+1),19]
  #CRM: separate out Maturity:
  MatAge_F<-data$endgrowth$Age_Mat[data$endgrowth$Sex==1&data$endgrowth$Bio_Pattern==GrowthMorph]
  MatAge_M<-data$endgrowth$Age_Mat[data$endgrowth$Sex==2&data$endgrowth$Bio_Pattern ==GrowthMorph]

  #CRM: separate out beg yr weight-at-age:
  Wt_Beg_F<-data$endgrowth$Wt_Beg[data$endgrowth$Sex==1 & data$endgrowth$Bio_Pattern==GrowthMorph]
  Wt_Beg_M<-data$endgrowth$Wt_Beg[data$endgrowth$Sex==2 & data$endgrowth$Bio_Pattern==GrowthMorph]

  ## selectivity at age for fishery
  if (GrowthMorph == 1) {
    sel_LY_Trawl_F<-subset(data$ageselex,Fleet==1&Yr==LY&Factor=="Asel2"&Morph==Morph1_F&Sex==1)
    sel_LY_Trawl_M<-subset(data$ageselex,Fleet==1&Yr==LY&Factor=="Asel2"&Morph==Morph1_M&Sex==2)

    wt_LY_Trawl_F<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt"&Morph==Morph1_F&Sex==1)
    wt_LY_Trawl_M<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt"&Morph==Morph1_M&Sex==2)
  }
  if (GrowthMorph == 2) {
    sel_LY_Trawl_F<-subset(data$ageselex,Fleet==1&Yr==LY&Factor=="Asel2"&Morph==Morph2_F&Sex==1)
    sel_LY_Trawl_M<-subset(data$ageselex,Fleet==1&Yr==LY&Factor=="Asel2"&Morph==Morph2_M&Sex==2)

    wt_LY_Trawl_F<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt"&Morph==Morph2_F&Sex==1)
    wt_LY_Trawl_M<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt"&Morph==Morph2_M&Sex==2)
  }

  #CRMcommentedout: sel_LY_Long<-subset(data$ageselex,data$ageselex$Fleet==2&data$ageselex$Yr==LY&data$ageselex$Factor=="Asel2")
  ## weight at age for two fisheries
  #wt_LY_Trawl<-subset(data$ageselex,Fleet==1 & Yr==LY & Factor=="bodywt")
  #CRMcommentedout: wt_LY_Long<-subset(data$ageselex,data$ageselex$Fleet==2&data$ageselex$Yr==LY&data$ageselex$Factor=="bodywt")
  ## numbers at age
  Nage_LY<-subset(data$natage,data$natage$"Beg/Mid"=="B"& Yr==LY & Area==area & Bio_Pattern==GrowthMorph)


  #Make this age RecAge recruits FY to LY
  ##age 1 recruits 1978 - (LY-1)
  rec_F<-as.numeric(data$natage[,(13+RecAge)][data$natage$Yr<=LY&data$natage$Yr>=FY&data$natage$Sex==1&data$natage$"Beg/Mid"=="B"&data$natage$Area==area&data$natage$Bio_Pattern==GrowthMorph])
  rec_M<-as.numeric(data$natage[,(13+RecAge)][data$natage$Yr<=LY&data$natage$Yr>=FY&data$natage$Sex==2&data$natage$"Beg/Mid"=="B"&data$natage$Area==area&data$natage$Bio_Pattern==GrowthMorph])
  Rec_1 <- rec_F + rec_M
  N_rec<-length(Rec_1)

  #SSB<-as.numeric(data$sprseries$SPB[data$natage$Yr<=LY&data$sprseries$Yr>=FY])
  if (Nmorph==1) {
    SSB<-as.numeric(data$timeseries$"SpawnBio"[data$timeseries$Yr<=LY&data$timeseries$Yr>=FY&data$timeseries])
  }
  if (GrowthMorph ==1 & Nmorph>1) {
    SSB<-as.numeric(data$timeseries$"SpawnBio_GP:1"[data$timeseries$Yr<=LY&data$timeseries$Yr>=FY&data$timeseries$Area==area])
    #SSB<-SSB[1:(LY-FY)]
  }

  if (GrowthMorph ==2 & Nmorph>1) {
    SSB<-as.numeric(data$timeseries$"SpawnBio_GP:2"[data$timeseries$Yr<=LY&data$timeseries$Yr>=FY&data$timeseries$Area==area])
  }

  #T1<-noquote(paste(data_file))
  T1 = noquote(paste(species))
  #write(T1,paste(data_file),ncolumns =  1 )
  write(T1,paste(data_file),ncolumns = 1)
  T1<-noquote(" 0 # SSL Species???")
  write(T1,paste(data_file),ncolumns = 1,append=T)
  T1<-noquote(" 0 # Constant Buffer Dorn?")
  write(T1,paste(data_file),append = T)
  T1<-noquote(" 1 # Number of fisheries")
  write(T1,paste(data_file),append = T)
  T1<-noquote(" 2 # Number of Sexes")
  write(T1,paste(data_file),append = T)
  T1<-noquote(paste(F_5,"# Average 5 Yr F"))
  write(T1,paste(data_file),append = T)
  T1<-noquote("1 # Author f")
  write(T1,paste(data_file),append = T)
  T1<-noquote("0.4 # SPR ABC")
  write(T1,paste(data_file),append = T)
  T1<-noquote("0.35 # SPR MSY")
  write(T1,paste(data_file),append = T)
  T1<-noquote("1 # Spawning month")
  write(T1,paste(data_file),append = T)

  T1<-noquote(paste0(NAGES-RecAge+1," # number of ages"))
  write(T1,paste(data_file),append = T)

  T1<-noquote("1 # Fratio")
  write(T1,paste(data_file),append = T)

  T1<-noquote("# natural mortality")
  write(T1,paste(data_file),append = T)
  write(M1,paste(data_file),append = T,ncolumns =  45)
  write(M2,paste(data_file),append = T,ncolumns =  45)## not sure what this is.

  T1<-noquote("# Maturity females")
  write(T1,paste(data_file),append = T)
  #Steve's trick: write(rep(1,NAGES),paste(data_file),append = T,ncolumns = 45) ## Female maturity??
  #CRM: straight up maturity
  write(round(as.numeric(MatAge_F[(1+RecAge):length(MatAge_F)]),4),paste(data_file),append=T,ncolumns = 45)

  T1<-noquote("# Maturity males")
  write(T1,paste(data_file),append = T)
  #Steve's trick: write(rep(1,NAGES),paste(data_file),append = T,ncolumns =  45)## Male maturity??
  #CRM: straight up maturity
  write(round(as.numeric(MatAge_M[(1+RecAge):length(MatAge_M)]),4),paste(data_file),append=T,ncolumns = 45)

  T1<-noquote("# wt spawn females")
  write(T1,paste(data_file),append = T)
  #Steve's write(round(as.numeric(WGT_F),4),paste(data_file),append = T,ncolumns =  45)
  #Carey's:
  write(round(as.numeric(Wt_Beg_F[(1+RecAge):length(Wt_Beg_F)]),4),paste(data_file),append = T,ncolumns =  45)

  T1<-noquote("# WtAge females by fishery")
  write(T1,paste(data_file),append = T)
  write(round(as.numeric(wt_LY_Trawl_F[(8+RecAge):(8+NAGES)]),4),paste(data_file),append = T,ncolumns =  45)
  #CRMcommentedout: write(round(as.numeric(wt_LY_Long[1,9:(NAGES+8)]),4),paste(data_file),append = T,ncolumns =  45)

  T1<-noquote("# WtAge males by fishery")
  write(T1,paste(data_file),append = T)
  write(round(as.numeric(wt_LY_Trawl_M[(8+RecAge):(8+NAGES)]),4),paste(data_file),append = T,ncolumns = 45)
  #CRMcommentedout: write(round(as.numeric(wt_LY_Long[2,9:(NAGES+8)]),4),paste(data_file),append = T,ncolumns = 45)

  T1<-noquote("# Selectivity females by fishery")
  write(T1,paste(data_file),append = T)
  write(round(as.numeric(sel_LY_Trawl_F[(8+RecAge):(8+NAGES)]),4),paste(data_file),append = T,ncolumns =  45)
  #CRMcommentedout: write(round(as.numeric(sel_LY_Long[1,9:(NAGES+8)]),4),paste(data_file),append = T,ncolumns =  45)

  T1<-noquote("# Selectivity males by fishery")
  write(T1,paste(data_file),append = T)
  write(round(as.numeric(sel_LY_Trawl_M[(8+RecAge):(8+NAGES)]),4),paste(data_file),append = T,ncolumns =  45)
  #CRMcommentedout: write(round(as.numeric(sel_LY_Long[2,9:NAGES+8]),4),paste(data_file),append = T,ncolumns =  45)

  T1<-noquote(paste0("# Numbers at age in ",LY," females males"))
  write(T1,paste(data_file),append = T)
  write(as.numeric(Nage_LY[1,(13+RecAge):(13+NAGES)]),paste(data_file),append = T,ncolumns =  45)
  write(as.numeric(Nage_LY[2,(13+RecAge):(13+NAGES)]),paste(data_file),append = T,ncolumns =  45)

  T1<-noquote("# No Recruitments")
  write(T1,paste(data_file),append = T)
  write(N_rec,paste(data_file),append = T,ncolumns =  45)

  T1<-noquote("# Recruitment")
  write(T1,paste(data_file),append = T)
  write(round(Rec_1,1),paste(data_file),append = T,ncolumns =  45)

  T1<-noquote(paste("# SSB ", FY,"-",LY,sep=""))
  write(T1,paste(data_file),append = T)
  write(SSB,paste(data_file),append = T,ncolumns =  45)
}



