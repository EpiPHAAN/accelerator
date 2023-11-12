#' Title
#'
#' @param intervals 
#' @param label 
#'
#' @return
#' @export
#'
#' @examples
genWhen_bedGGIR=function(intervals,label="bed"){
  intervals %>% pluck(label) %>% mutate(day=as.Date(from+difftime(to,from)*.6),label=label) 
}
