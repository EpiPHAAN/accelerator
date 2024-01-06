#' @title Convert an interval to a dataframe
#' @description Convert an interval to a dataframe
#' @param intervals
#' @return
#' @export
#' @examples
interval2Dataframe=function(intervals){
  result=intervals %>% enframe() %>% unnest(value) %>% mutate(start=int_start(value),end=int_end(value)) %>% select(-value) 
  if(is.null(names(intervals))) return(result %>% select(-name))  
  result %>% mutate(name=str_replace(name,"\\.\\.[0-9]+$",""))
}