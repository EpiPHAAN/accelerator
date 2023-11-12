#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
ggir2What=function(df){
  cat("\nComputing 'WHAT' intervals using GGIR epochs\n")
  #pb <- progress_estimated(nrow(df))
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) :eta", total = nrow(df))
  
  ggir2IntervalWithProgress <- function(...){
    pb$tick()
    ggir2Interval(...)
  }
  
  pmap(df,ggir2IntervalWithProgress)
}