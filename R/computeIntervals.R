#roxygen
#' @title computeIntervals
#' @description computeIntervals
#' @param dataframe
#' @param defineIntervals
#' @return
#' @export
#' @examples
computeIntervals <- function(dataframe,defineIntervals){
    computingIntervals=accelerometry2Intervals %>% partial(defineIntervals = defineIntervals)
  future_pmap( dataframe,computingIntervals)
}
