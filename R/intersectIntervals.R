
#' Title intersectIntervals
#'
#' @param what_n 
#' @param when_n 
#' @param useNames 
#' @param short 
#'
#' @return
#' @export
#'
#' @examples
intersectIntervals<-function(what_n,when_n,useNames=FALSE,short=TRUE){

  #Codigo para cuando el segundo parámetro es una lista de intervalos
  if (is.list(when_n)){
    return(map(when_n, ~intersectIntervals(what_n, .x, useNames=useNames,short=short)))
  }


  #Codigo para cuando el primer parámetro es una lista de intervalos
  if (is.list(what_n)){
    return(map(what_n, ~intersectIntervals(.x, when_n,useNames=useNames,short=short)))
  }

  #Codigo principal para el caso de intersectar un vector de intervalos con un vector de intervalos con nombre
  if(useNames & !is.null(names(when_n))){
  resultado=map2(when_n, names(when_n), function(.x,.y) intersecta_1n(what_n=what_n,when_1=.x,name=.y, short=short))  
  } else {
  resultado=when_n %>% map(intersecta_1n %>% partial(what_n=what_n, short=short)) %>% set_names(NULL)
  }
  
  if(short) resultado=resultado %>% keep(negate(is.null))

  resultado=vectorInterval(resultado)
  resultado



}


int_overlaps_strict <- function(int1, int2) {
  stopifnot(c(is.interval(int1), is.interval(int2)))
  int1 <- int_standardize(int1)
  int2 <- int_standardize(int2)
  int1@start < int2@start + int2@.Data & int2@start < int1@start + int1@.Data
}

intersecta_1n<-function(what_n,when_1,name=NULL,short=TRUE){
  
  if(!is.null(names(what_n))) names(what_n)=NULL
  indices=when_1 %>% int_overlaps_strict(what_n) %>% which()
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


#' Title touchedIntervals
#'
#' @param what_n 
#' @param when_n 
#' @param useNames 
#' @param short 
#' @param operator 
#'
#' @return
#' @export
#'
#' @examples
touchedIntervals<-function(what_n,when_n,useNames=FALSE,short=TRUE,operator=I){

    
  #Codigo para cuando el segundo parámetro es una lista de intervalos
  if (is.list(when_n)){
    return(map(when_n, ~touchedIntervals(what_n, .x, useNames=useNames,short=short,operator=operator)))
  }
  
  #Codigo para cuando el primer parámetro es una lista de intervalos
  if (is.list(what_n)){
    return(map(what_n, ~touchedIntervals(.x, when_n,useNames=useNames,short=short,operator=operator)))
  }
  
  #Codigo principal para el caso de intersectar un vector de intervalos con un vector de intervalos con nombre
  if(useNames & !is.null(names(when_n))){
    resultado=map2(when_n, names(when_n), function(.x,.y) touchedIntervals_1n(what_n=what_n,when_1=.x,name=.y, short=short,operator=operator))  
  } else {
    resultado=when_n %>% map(touchedIntervals_1n %>% partial(what_n=what_n, short=short,operator=operator)) %>% set_names(NULL)
  }
  
  if(short) resultado=resultado %>% keep(negate(is.null))
  
  resultado=vectorInterval(resultado)
  unique(resultado)
  
}



touchedIntervals_1n<-function(what_n,when_1,operator,name=NULL,short=TRUE){
   
   if(!is.null(names(what_n))) names(what_n)=NULL
   indices=when_1 %>% int_overlaps_strict(what_n) %>% which()
 
   if(length(indices)==0){
     if(short) return (NULL)
     return(interval(when_1@start,when_1@start))
   }
   resultado=what_n[indices]  
   if (!is.null(name) & length(name)==1)    names(resultado) = sprintf(".%d", seq_along(resultado))
   unique(resultado) 
 }
 
 vectorInterval=function(lista){
   do.call(c,lista) 
 }
 
