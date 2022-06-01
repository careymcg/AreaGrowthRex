
#' Get Age Data and Fits from SS Model Runs
#'
#' @param ssruns the Stock Synthesis runs to include in the data frame
#' @param mnames names of the Stock Synthesis runs
#' @return
#' @export
#'
#' @examples
get_ages <- function(ssruns, mnames) {

  nmodel <- length(ssruns)
  big.df <- data.frame()

  for (imodel in 1:nmodel) {
    numrow<-nrow(ssruns[[imodel]]$agedbase)
    if (numrow>0) {
      ssruns[[imodel]]$agedbase$mname <- mnames[imodel] # make a variable out of the model name

      if (nrow(big.df)==0) {
        big.df <- ssruns[[imodel]]$agedbase
      } else {
        big.df <- rbind(big.df, ssruns[[imodel]]$agedbase)
      }
    }
  }
  big.t <- tibble::as_tibble(big.df)
  return(big.t)
}
