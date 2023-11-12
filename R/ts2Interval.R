#' Title
#'
#' @param ts 
#' @param nucleos 
#'
#' @return
#' @export
#'
#' @examples
ts2Interval <- function(ts,nucleos=numCores(4),...){
  resultado=parallel::mclapply(currentProject$defineWhat,function(x)x(ts,...)[["intervals"]],mc.cores=nucleos)
  names(resultado)=names(currentProject$defineWhat)

  resultado %>% keep(function(df) is.data.frame(df))
 
 
}