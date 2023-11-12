#' Title
#'
#' @param df 
#' @param q 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
quantileForIntervals=function(df,q=0.5,...) {
  timezone=tz(df$from)
  intervalsQuantiles(df,q) %>% 
    select(day,from,to,quantile) %>%
    mutate(reference=ymd_h(str_c(day," 00"),tz=timezone)) %>% 
    mutate(hour=difftime(quantile,reference,units="hours")) %>%
    select(from,to,reference,quantile,hour)
}
