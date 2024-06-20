#roxygen
#' @title computeIntervals
#' @description computeIntervals
#' @param dataframe
#' @param defineIntervals
#' @return
#' @export
#' @examples
computeIntervals <- function(dataframe,defineIntervals){

 pmap(dataframe,accelerometry2Intervals %>% partial(defineIntervals=defineIntervals),.progress = "intervals")
}

