#' Title
#'
#' @param df 
#' @param currentProject 
#'
#' @return
#' @export
#'
#' @examples
ggir2WhatV2=function(df,currentProject){
  cat("\nComputing 'WHAT' intervals using GGIR epochs\n")
  #pb <- progress_estimated(nrow(df))
  pb <- progress::progress_bar$new(format = ":MSG [:bar] :current/:total (:percent) :eta", total = nrow(df))
  ggir2IntervalWithProgress <- function(base,RAW,...){
    #pb$tick()$print()
    pb$tick(tokens = list(MSG = RAW))
    ggir2IntervalV2(currentProject,base,RAW,...)
  }
  
  pmap(df,ggir2IntervalWithProgress)
}