#' Title
#'
#' @param df 
#' @param what 
#'
#' @return
#' @export
#'
#' @examples
getWhatL2 <- function(df,what="what") {
  cat("\nComputing 'WhatL2' intervals\n")
  
  pb <- progress::progress_bar$new(format = ":MSG [:bar] :current/:total (:percent) :eta", total = nrow(df))
  getWhatWithProgress <- function(intervals,idBIN){
    pb$tick(tokens = list(MSG = str_c(idBIN)))
    currentProject$defineWhatL2(intervals, idBIN)
  }
  
  
  pmap( list(intervals=pluck(df,"what"), idBIN=pluck(df,"RAW")) ,.f = getWhatWithProgress)
}
