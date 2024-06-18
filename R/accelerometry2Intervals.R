#' @title accelerometry2Intervals
#' @description accelerometry2Intervals
#' @param driver
#' @param defineIntervals
#' @return
#' @export
#' @examples
accelerometry2Intervals<-function(driver,defineIntervals,activity_log,...){
  # New version that allows self referencing functions
  dependences=defineIntervals$.dependences
  defineIntervals=defineIntervals %>% keep(is.function)
  
  data=driver()
  oldNumNULLS <- length(defineIntervals)
  result <- map(defineIntervals,~NULL)
  contador=0
  
  
  repeat{
    contador=contador+1
    message("Pasada: ",contador)
    for(v in names(result)){
      if(is.null(result[[v]])){
        message("Calculo de ", v)
        intervalsForThis=list()
        if(!is.null(dependences[[v]])){
          intervalsForThis= result[dependences[[v]]]#result %>% #keep({names(.) %in% dependences[[v]]})
        }
        tryCatch({
        result[[v]] <- (defineIntervals[[v]])(df=data,intervals=intervalsForThis,activity_log=activity_log,...)
        if(!is.null(result[[v]])){
          message("\tIntervalos: ", length(result[[v]]))}
        }, error=function(e) {NULL})
      }
    }
    
    newNumNULLS=sum(map_lgl(result,is.null),na.rm=T)
    
    FIN <- newNumNULLS==0 | newNumNULLS>=oldNumNULLS
    
    message("FIN? ",FIN)
    if(FIN) break
    oldNumNULLS=newNumNULLS
  }
  result %>% keep(negate(is.null)) 
}


# Simpler old version
#accelerometry2Intervals<-function(driver,defineIntervals,...){
#  data=driver()
#  map(defineIntervals,function(f) tryCatch({ f(data)}, error=function(e) {NULL})) %>% keep(negate(is.null)) 
#}