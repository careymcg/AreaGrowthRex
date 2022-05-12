


plot_age_compare <- function(ssruns, narea, mnames) {

    for (imodel in 1:nmodel) {
      numrow<-nrow(ssruns[[imodel]]$agedbase)
      if (numrow>0) {
        ssruns[[imodel]]$agedbase$mname <- mnames[imodel] # make a variable out of the model name

        if (nrow(biglen.df)==0) {
          biglen.df <- ssruns[[imodel]]$agedbase
        } else {
          biglen.df <- rbind(biglen.df, ssruns[[imodel]]$agedbase)
        }
      }
    }


}


