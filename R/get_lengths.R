
#' Get length data and fits from ss model runs
#'
#' @param ssruns the Stock Synthesis runs to include in the data frame
#' @param mnames the names of the Stock Synthesis runs
#'
#' @return
#' @export
#'
#' @examples
get_lengths<-function(ssruns,mnames) {

  nmodel <- length(ssruns)
  biglen.df <- data.frame()

    for (imodel in 1:nmodel) {
      ssruns[[imodel]]$lendbase$mname <- mnames[imodel] # make a variable out of the model name
      if (imodel == 1) {
        biglen.df <- ssruns[[1]]$lendbase
      } else {
        biglen.df <- rbind(biglen.df, ssruns[[imodel]]$lendbase)
      }
    }
    # Filter by number of areas (different plots for 1 vs 2 area models)
    biglen.t <- tibble::as_tibble(biglen.df)
    return(biglen.t)
}
