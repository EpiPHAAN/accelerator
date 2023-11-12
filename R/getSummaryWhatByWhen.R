#' Title
#'
#' @param what 
#' @param when 
#'
#' @return
#' @export
#'
#' @examples
getSummaryWhatByWhen=function(what,when){
  cat("\nComputing summaries of 'WHAT' inside `WHEN` \n")
  
  #pb <- progress_estimated(length(what))
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) :eta", total = length(what))
  summaryWhatByWhenWithProgress=function(...){
    #pb$tick()$print()
    pb$tick()
    summaryWhatByWhen(...)
  }
    
  
  map2(what,when,summaryWhatByWhenWithProgress)
  }