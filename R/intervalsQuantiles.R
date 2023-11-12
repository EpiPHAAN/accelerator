timeQuantile_1=function(intervals,probs=0.5){
  intervals %>% filter(probs<=probTo & probs>probFrom) %>%
    mutate(probs=probs) %>%
    mutate(quantile=from+difftime(to,from)*((probs-probFrom)/(probTo-probFrom)))
}


#' Title
#'
#' @param intervals 
#' @param probs 
#'
#' @return
#' @export
#'
#' @examplesR
intervalsQuantiles=function(intervals,probs=c(0.5)){
  tmp=intervals %>% as_tibble() %>%
    mutate(duracion=difftime(to,from,units="secs"),
           probTo=cumsum(as.integer(duracion))/sum(as.integer(duracion)),
           probFrom=(function(x)c(-1e-9,x[-length(x)]))(probTo)
    )
  
  reduce(map(probs,function(probs)timeQuantile_1(tmp,probs)),rbind)
}
