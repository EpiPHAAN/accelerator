#' Title
#'
#' @param base 
#' @param RAW 
#' @param start 
#' @param end 
#' @param progreso 
#'
#' @return
#' @export
#'
#' @examples
ggir2ts <- function(base,RAW,start=NA,end=NA,progreso=NULL){
  #cat(base,":", RAW)
  #if(!is.na(start) & !is.na(end)) cat(" [",as.character(start),"=>",as.character(end),"]\n")
  getGGIR(base,RAW,start=start,end=end)$FASE1
}
