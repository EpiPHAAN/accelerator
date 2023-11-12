#' Title
#'
#' @param nCores 
#'
#' @return
#' @export
#'
#' @examples
numCores=function(nCores){
   if(Sys.info()[['sysname']]=="Windows") 1 else nCores 
}