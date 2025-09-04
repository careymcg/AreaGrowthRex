#' Organize index data from multiple runs and make a tibble
#'
#' @param ssruns
#' @param mlabels
#'
#' @return
#' @export
#'
#'@import dplyr
#' @examples
get_index_data<-function(ssruns,mlabels) {
  index.t<-tibble()
  #fleets<-unique(ssruns[[i]]$cpue$Fleet)

  for (i in 1:length(mlabels)) {
    index1<-ssruns[[i]]$cpue %>%
      drop_na(Like) %>% select(c(year = Yr, obs = Obs, exp = Exp, se = SE, fleet = Fleet_name)) %>%
      mutate(model = mlabels[i])
    if (i == 1) {
      index.t <-index1
    } else {
      index.t <-rbind(index.t,index1)
    }
  }

  #calculate lognormal quantiles:
  index.t <- index.t %>% mutate(lower = qlnorm(0.025,meanlog = log(obs),sdlog = se),
                                upper = qlnorm(1-0.025,meanlog = log(obs),sdlog = se))

  return(index.t)
}
