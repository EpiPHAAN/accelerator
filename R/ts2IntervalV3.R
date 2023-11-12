#' Title
#'
#' @param ts 
#' @param currentProject 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ts2IntervalV3 <- function(ts,currentProject,...){
  resultado=map(currentProject$defineWhat,function(x)x(ts,...)[["intervals"]])
  names(resultado)=names(currentProject$defineWhat)

  resultado %>% keep(function(df) is.data.frame(df))
}