#' Title
#'
#' @param currentProject 
#' @param base 
#' @param RAW 
#' @param start 
#' @param end 
#'
#' @return
#' @export
#'
#' @examples
ggir2IntervalV3<- function(currentProject,base,RAW,start=NA,end=NA) {
  ggir2ts(base,RAW,start,end) %>% ts2IntervalV3(currentProject,...)
}