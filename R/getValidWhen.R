#' Title
#'
#' @param when 
#' @param invalidWhen 
#'
#' @return
#' @export
#'
#' @examples
getValidWhen=function(when,invalidWhen){
  cat("\nComputing valid 'WHEN' intervals\n")
  #pb <- progress_estimated(length(when))
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) :eta", total = length(when))
  
  cleanInvalidWhenWithProgress <- function(...){
    #pb$tick()$print()
    pb$tick()
    cleanInvalidWhen(...)
  }
  
  map2(when,invalidWhen,cleanInvalidWhenWithProgress)
}
