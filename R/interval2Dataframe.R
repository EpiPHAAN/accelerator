#' @title Convert an interval to a dataframe
#' @description Convert an interval to a dataframe
#' @param intervals
#' @return
#' @export
#' @examples
interval2Dataframe=function(intervals, .id=c()){
  #Por si llega vacia
  if(is.null(intervals)) return(data.frame())
  
  #Por si hay que añadir columnas con nombres
      if(length(.id)>0) {
        thisId=.id[1]
        .id=.id[-1]
      } else {thisId=c()
             .id=c()}

  #Para el caso de una lista
   if (is.list(intervals)){
     result=map(intervals, interval2Dataframe,.id=.id)
     if(length(thisId)==1) result=bind_rows(result,.id=thisId)
     return (result)
  }
  
  # Para el caso del tipo base
  result=intervals %>% enframe() %>% unnest(value) %>% mutate(start=int_start(value),end=int_end(value)) %>% select(-value) 
  if(is.null(names(intervals))) return(result %>% select(-name)) 
  result %>% mutate(name=str_replace(name,"\\.\\.[0-9]+$",""))
  #result
}