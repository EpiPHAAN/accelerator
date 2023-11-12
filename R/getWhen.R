#' Title
#'
#' @param df 
#' @param what 
#'
#' @return
#' @export
#'
#' @examples
getWhen <- function(df,what="what") {
  cat("\nComputing 'WHEN' intervals\n")
  
  #pb <- progress_estimated(nrow(df))
  pb <- progress::progress_bar$new(format = ":MSG [:bar] :current/:total (:percent) :eta", total = nrow(df))
  getWhenWithProgress <- function(intervals,idBIN){
    #pb$tick()$print()
    pb$tick(tokens = list(MSG = str_c(idBIN)))
    currentProject$defineWhen(intervals, idBIN)
  }
  
  
  pmap( list(intervals=pluck(df,"what"), idBIN=pluck(df,"RAW")) ,.f = getWhenWithProgress)
}