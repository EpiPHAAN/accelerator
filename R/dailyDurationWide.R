#' Title
#'
#' @param whenList 
#' @param whatList 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dailyDurationWide=function(whenList,whatList,...){
  dailyDuration(whenList,whatList) %>% pivot_wider(names_from="what",values_from="duration")
}