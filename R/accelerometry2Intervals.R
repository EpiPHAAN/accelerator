#' @title accelerometry2Intervals
#' @description accelerometry2Intervals
#' @param driver
#' @param defineIntervals
#' @return
#' @export
#' @examples
accelerometry2Intervals<-function(driver,defineIntervals,...){
  data=driver()
  map(defineIntervals,function(f) tryCatch({ f(data)}, error=function(e) {NULL})) %>% keep(negate(is.null)) 
}