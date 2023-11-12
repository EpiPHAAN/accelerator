#' Title
#'
#' @param when 
#' @param invalidWhen 
#'
#' @return
#' @export
#'
#' @examples
cleanInvalidWhen=function(when,invalidWhen){
  invalidWhen=invalidWhen %>% rename(label=when)
  when %>% map(~ .x %>% left_join(invalidWhen,by=c("day","label")) %>% filter(is.na(reason)) %>% select(-reason) )  
}
