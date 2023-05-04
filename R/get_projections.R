
#' Write the data file for running projections using AFSC projection model code
#'
#' @param mydir
#' @param MyOutput
#' @param NagesProject
#' @param LastYr
#' @param RecruitmentAge
#' @param species
#'
#' @return
#' @export
#'
#' @examples
get_projections<-function(mydir,MyOutput,NagesProject,FirstYr,LastYr,RecruitmentAge,species) {

#write the data file within the folder for the ss run
  if (MyOutput$ngpatterns > 1) {
    #NOTE: NAME ALL DATA FILES STARTING WITH "ForProjections"
    write_proj_ss_2Area(data_file=file.path(mydir,"ForProjections_GrowthMorph1.dat"),data=MyOutput,GrowthMorph=1,area =1,Narea = 2,Nmorph = 2,NAGES=NagesProject,FY=FirstYr,LY=LastYr,RecAge = RecruitmentAge,species = species)
    write_proj_ss_2Area(data_file=file.path(mydir,"ForProjections_GrowthMorph2.dat"),data=MyOutput,GrowthMorph=2,area =2,Narea = 2,Nmorph = 2,NAGES=NagesProject,FY=FirstYr,LY=LastYr,RecAge = RecruitmentAge,species = species)

  } else {
    #NOTE: NAME ALL DATA FILES STARTING WITH "ForProjections"
    write_proj_ss_1Area(data_file=file.path(mydir,"ForProjections.dat"),data=MyOutput,NAGES=NagesProject,FY=FirstYr,LY=LastYr,RecAge = RecruitmentAge,species = species)
  }

  #could add my code to this for creating file structure for running projections

}
