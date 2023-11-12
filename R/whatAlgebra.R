#' Title
#'
#' @param what1 
#' @param what2 
#'
#' @return
#' @export
#'
#' @examples
whatIntersect=function(what1,what2){
  if(is.null(what1) ||is.null(what2)){
    data.frame(from=now(),to=now()) %>% filter(to<from) %>% as_tibble()
  }
  else{
    what1 %>% intervalIntersectv2(what2)  %>% transmute(from=fromNew,to=toNew)
  }
}


#' Title
#'
#' @param whatInterval 
#'
#' @return
#' @export
#'
#' @examples
whatComplementaryInternal=function(whatInterval){
  if(is.null(whatInterval) || is.data.frame(whatInterval) & nrow(whatInterval)==0) {
    data.frame(from=now(),to=now()) %>% filter(to<from) %>% as_tibble()}
  else{
    whatInterval  %>%
      transmute(from2=to+dseconds(1),to2=lead(from,1)+dseconds(-1)) %>%
      filter(row_number()>1 & row_number()<nrow(.) | row_number()==1) %>%
      filter(!is.na(to2)) %>% transmute(from=from2,to=to2)
  }
}


#' Title
#'
#' @param whatInterval 
#' @param intervalSpace 
#'
#' @return
#' @export
#'
#' @examples
whatComplementary=function(whatInterval,intervalSpace){
  start=intervalSpace$from %>% first()
  end=intervalSpace$to %>% last()
  timezone=tz(start)
  
  interno=whatComplementaryInternal(whatInterval)
  
  primero=data.frame(from=start,to=whatInterval$from %>% first()-dseconds(1)) %>%
    filter(from<to) 
  
  ultimo=data.frame(from=whatInterval$to %>% last()+dseconds(1),to=end)%>%
    filter(from<to) 
  rbind(primero,interno,ultimo) %>% whatIntersect(intervalSpace)
}





#' Title
#'
#' @param intervalSpace 
#' @param intervalRemove 
#'
#' @return
#' @export
#'
#' @examples
whatDifference=function(intervalSpace,intervalRemove){
  intervalRemove %>% whatComplementaryInternal() %>% whatIntersect(intervalSpace) 
}
