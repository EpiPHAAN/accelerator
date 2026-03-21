timeQuantile_1=function(intervals,probs=0.5){
  intervals %>% filter(probs<=probTo & probs>probFrom) %>%
    mutate(probs=probs) %>%
    mutate(quantile=from+difftime(to,from)*((probs-probFrom)/(probTo-probFrom)))
}


#' @title Calculate Temporal Quantiles of Intervals
#'
#' @description Computes cumulative temporal quantiles across a collection of intervals. It determines the exact timestamp where the specified duration proportions (\code{probs}) of the total interval time are reached.
#'
#' @param intervals A data frame representing the intervals (with \code{from} and \code{to} columns).
#' @param probs A numeric vector of probabilities (quantiles) with values between 0 and 1. Default is 0.5 (median).
#'
#' @return A data frame containing the corresponding timestamp quantiles for the overall duration of the provided intervals.
#' @export
#'
#' @examples
intervalsQuantiles=function(intervals,probs=c(0.5)){
  tmp=intervals %>% as_tibble() %>%
    mutate(duracion=difftime(to,from,units="secs"),
           probTo=cumsum(as.integer(duracion))/sum(as.integer(duracion)),
           probFrom=(function(x)c(-1e-9,x[-length(x)]))(probTo)
    )
  
  reduce(map(probs,function(probs)timeQuantile_1(tmp,probs)),rbind)
}
