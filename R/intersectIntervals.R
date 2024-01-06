#' @title intersectIntervals
#' @description intersectIntervals
#' @param what_n
#' @param when_n
#' @param useNames
#' @param short
#' @return
intersectIntervals<-function(what_n,when_n,useNames=FALSE,short=TRUE){
  if(useNames & !is.null(names(when_n))){
  resultado=map2(when_n, names(when_n), function(.x,.y) intersecta_1n(what_n=what_n,when_1=.x,name=.y, short=short))  
  } else {
  resultado=when_n %>% map(intersecta_1n %>% partial(what_n=what_n, short=short)) %>% set_names(NULL)
  }
  
  if(short) resultado=resultado %>% keep(negate(is.null))

  resultado=vectorInterval(resultado)
  resultado
}

intersecta_1n<-function(what_n,when_1,name=NULL,short=TRUE){
  indices=when_1 %>% int_overlaps(what_n) %>% which()
  if(length(indices)==0){
    if(short) return (NULL)
    return(interval(when_1@start,when_1@start))
  }
  resultado=what_n[indices] %>% lubridate::intersect(when_1)
  if (!is.null(name) & length(name)==1)    names(resultado) = sprintf(".%d", seq_along(resultado))
  resultado
}

vectorInterval=function(lista){
  do.call(c,lista) 
}

int_overlaps_numeric <- function (int1, int2) {
  stopifnot(c(is.interval(int1), is.interval(int2)))

  x <- intersect(int1, int2)
  if(is.na(x@.Data)) return(NULL)
  x
}