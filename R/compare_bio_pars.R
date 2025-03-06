#' Title
#'
#' @param ssruns
#' @param mlabels
#'
#' @return
#' @export
#'
#'@import dplyr
#'@importFrom tidyr pivot_wider
#'
#' @examples
compare_bio_pars<-function(ssruns,mlabels) {
  bio_table.t<-tibble()

  for (i in 1:length(ssruns)) {
    bio<-ssruns[[i]]$"parameters" %>% select(c(label = Label, estimate = Value, stdev = Parm_StDev)) %>%
      filter(label == "NatM_uniform_Fem_GP_1" |
             label == "NatM_uniform_Mal_GP_1" |
            label == "NatM_uniform_Fem_GP_2" |
              label == "NatM_uniform_Mal_GP_2" |
              label == "L_at_Amin_Fem_GP_1" |
              label == "L_at_Amax_Fem_GP_1" |
              label == "L_at_Amin_Mal_GP_1" |
              label == "L_at_Amax_Mal_GP_1" |
               label == "L_at_Amin_Fem_GP_2" |
              label == "L_at_Amax_Fem_GP_2" |
              label == "L_at_Amin_Mal_GP_2" |
              label == "L_at_Amax_Mal_GP_2" |
              label == "VonBert_K_Fem_GP_1" |
              label == "VonBert_K_Mal_GP_1" |
              label == "VonBert_K_Fem_GP_2" |
              label == "VonBert_K_Mal_GP_2" |
              label == "CV_young_Fem_GP_1" |
              label == "CV_young_Mal_GP_1" |
              label == "CV_young_Fem_GP_2" |
              label == "CV_young_Mal_GP_2" |
              label == "CV_old_Fem_GP_1" |
              label == "CV_old_Mal_GP_1" |
              label == "CV_old_Fem_GP_2" |
              label == "CV_old_Mal_GP_2" |
              label == "RecrDist_GP_2_area_2_month_1" |
              label == "SR_LN(R0)" |
              label == "LnQ_base_NonEasternSurvey(2)" ) %>%
      mutate(model= mlabels[i])
    if (i == 1) {
      bio_table.t<-bio
    } else {
      bio_table.t<-rbind(bio_table.t,bio)
    }
  }

   bio_wide.t<-bio_table.t %>% pivot_wider(names_from = model,values_from = c(estimate,stdev))

return(bio_wide.t)
}
