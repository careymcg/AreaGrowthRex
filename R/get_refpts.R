
#' Grab the results of AFSC projections for biology and fishery ref pts
#'
#' @param ProjDir
#' @param LastYr
#'
#' @return
#' @export
#'
#' @examples
get_refpts<-function(ProjDir,LastYr) {

#Projected Total biomass:
means.file = file.path(ProjDir,"means.out")
means.out <-readLines(means.file)
therow = grep("Alternative 1 Biomass",means.out)
#Tbio.m <-matrix(scan(file = means.file,skip = therow + 3,nlines = 14),ncol =2,byrow = T)
Tbio.df = read.table(file = means.file,skip = therow + 3,nrows = 14, header = F)

Tbproj1 = round(1000*Tbio.df$V2[Tbio.df$V1==LastYr+1],digits = 0)
Tbproj2 = round(1000*Tbio.df$V2[Tbio.df$V1 == LastYr+2],digits = 0)

#Spawning biomass info:
percentiles.file = file.path(ProjDir,"percentiles.out")
Alt1.sb.df<-read.table(file = percentiles.file,skip = 24,nrows = 14,header = T)

SbUCIproj1 = round(1000*Alt1.sb.df$UpperCI_SSB[Alt1.sb.df$Year==LastYr+1],digits = 0)
SbUCIproj2 = round(1000*Alt1.sb.df$UpperCI_SSB[Alt1.sb.df$Year==LastYr+2],digits = 0)

Sbproj1 = round(1000*Alt1.sb.df$Mean_SSB[Alt1.sb.df$Year==LastYr+1],digits = 0)
Sbproj2 = round(1000*Alt1.sb.df$Mean_SSB[Alt1.sb.df$Year==LastYr+2],digits = 0)

SbLCIproj1 = round(1000*Alt1.sb.df$LowCI_SSB[Alt1.sb.df$Year==LastYr+1],digits = 0)
SbLCIproj2 =  round(1000*Alt1.sb.df$LowCI_SSB[Alt1.sb.df$Year==LastYr+2],digits = 0)

# sB100proj1 = round(1000*Alt1.sb.df$SSB100[Alt1.sb.df$Year==LastYr+1],digits = 0)
# sB100proj2 = round(1000*Alt1.sb.df$SSB100[Alt1.sb.df$Year==LastYr+1],digits = 0)

Brefs.df = read.table(percentiles.file,skip = 1,nrows = 1,header=T)
B100 = round(1000*Brefs.df$SB0,digits = 0)
B40 = round(1000*Brefs.df$SB40,digits = 0)
B35 = round(1000*Brefs.df$SB35,digits = 0)

Frefs.df = read.table(file = percentiles.file,skip = 41,nrows = 1,header = T)

F35 = round(Frefs.df$Fofl,digits = 2)
Fabc = round(Frefs.df$Fabc,digits = 2)

F35precise = Frefs.df$Fofl
B35precise = 1000*Brefs.df$SB35
F40precise = Frefs.df$Fabc
B40precise = 1000*Brefs.df$SB40

#---------------------------
bigfile.df = read.table(file = file.path(ProjDir,"bigfile.out"),header = T)

mean.bigfile.df = aggregate(x = list(CATCH = bigfile.df$Catch,Fmort = bigfile.df$F,SSB = bigfile.df$SSB,OFL = bigfile.df$OFL,ABC = bigfile.df$ABC),by = list(Year = bigfile.df$Yr,Alt = bigfile.df$Alternative),FUN = mean)
OFL_2_Yrs = round(1000*mean.bigfile.df$OFL[mean.bigfile.df$Year==LastYr+2 & mean.bigfile.df$Alt==1],digits = 0)
ABC_2_Yrs = round(1000*mean.bigfile.df$ABC[mean.bigfile.df$Year==LastYr+2 & mean.bigfile.df$Alt==1], digits = 0)

OFL_Next_Yr = round(1000*mean.bigfile.df$OFL[mean.bigfile.df$Year==LastYr+1 & mean.bigfile.df$Alt==1],digits = 0)
ABC_Next_Yr = round(1000*mean.bigfile.df$ABC[mean.bigfile.df$Year==LastYr+1 & mean.bigfile.df$Alt==1],digits = 0)

#------------------------
Exec.df = data.frame(Proj1 = c(Tbproj1,Sbproj1,
                               B100,B40,B35,F35,Fabc,Fabc,OFL_Next_Yr,ABC_Next_Yr,ABC_Next_Yr),
                     Proj2 = c(Tbproj2,Sbproj2,
                               B100,B40,B35,F35,Fabc,Fabc,OFL_2_Yrs,ABC_2_Yrs,ABC_2_Yrs))

write.csv(Exec.df,file = file.path(ProjDir,"ExecSumm_Table.csv"))

#--------------------------------------------------------------------------------------------
#Make a table for the harvest projections section
HarvestRecs.df = data.frame(Names = c(paste0("SSB_",LastYr+1),"B40","F40","maxFabc","B35","F35","Fofl"),
                            Values = c(Sbproj1,B40,Fabc,Fabc,B35,F35,F35))
write.csv(HarvestRecs.df,file = file.path(ProjDir,"HarvestRecs_Table.csv"))
#---------------------------------------------------------------------------------------------

#Pull only two values: ABC and F40:
MSTable.df<-data.frame(Names = c("F40","ABC_Next_Yr"),
                    Values = c(Fabc,ABC_Next_Yr))
write.csv(MSTable.df,file = file.path(ProjDir,"MSTable.csv"))
return(MSTable.df)

}
