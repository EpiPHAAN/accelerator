#' Title
#'
#' @param summary 
#' @param whatCond 
#' @param whenInvalid 
#' @param reason 
#'
#' @return
#' @export
#'
#' @examples
invalidBedOfFirstDay=function(summary, whatCond="isOn",whenInvalid="bed",reason="1stDayBed"){
  summary %>% filter(when=="daily", what==whatCond)  %>% summarise_all(first) %>% filter(hfrom>1) %>%
    select(day) %>% mutate(when=whenInvalid,reason=reason)
}
