#' @title Convert Accelerometry Data into Activity Intervals
#' 
#' @description 
#' Processes accelerometry data to generate a structured list of intervals. 
#' It employs an iterative "dependency resolver" approach, allowing interval-defining 
#' functions to reference results from one another. The function repeatedly attempts 
#' to resolve intervals that return `NULL` until all are calculated or no further 
#' progress can be made.
#' 
#' @param driver Function. A data loader or "factory" function that, when called 
#' without arguments `driver()`, returns the primary accelerometry data frame.
#' @param defineIntervals List. A list containing functions used to define intervals. 
#' While it may include a `.dependences` element, only the elements that are 
#' functions will be processed. Each function must accept the arguments 
#' `df`, `intervals`, `activity_log`, and `...`.
#' @param activity_log Data frame. A log of activities used as a reference for 
#' calculating specific time windows or intervals.
#' @param ... Additional arguments passed down to the individual interval functions.
#' 
#' @details 
#' The algorithm uses a `repeat` loop to iterate over the functions in `defineIntervals`. 
#' If a function fails or returns `NULL`, the system assumes a dependency is 
#' currently missing and will retry in the next iteration. 
#' 
#' Convergence is reached when:
#' 1. All intervals are successfully resolved (no `NULL` values remain).
#' 2. A complete pass over the functions results in no new resolved intervals 
#' (indicating a circular dependency or an unresolvable error).
#' 
#' @return A list containing only the successfully calculated intervals. Any 
#' elements that remained `NULL` after the process terminates are filtered out.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # Basic usage example:
#' results <- accelerometry2Intervals(
#'   driver = function() my_data_loading_func(),
#'   defineIntervals = my_interval_logic_list,
#'   activity_log = user_log
#' )
#' }
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
#    message("Pasada: ",contador)
    for(v in names(result)){
      if(is.null(result[[v]])){
#       message("Calculo de ", v)
        tryCatch({
        result[[v]] <- (defineIntervals[[v]])(df=data, intervals=result, activity_log=activity_log,...)
#       if(!is.null(result[[v]])){message("\tIntervalos: ", length(result[[v]]))}
        }, error=function(e) {NULL})
      }
    }
    
    newNumNULLS=sum(map_lgl(result,is.null),na.rm=T)
    
    FIN <- newNumNULLS==0 | newNumNULLS>=oldNumNULLS
    
#    message("FIN? ",FIN)
    if(FIN) break
    oldNumNULLS=newNumNULLS
  }
  result %>% keep(negate(is.null)) 
}
