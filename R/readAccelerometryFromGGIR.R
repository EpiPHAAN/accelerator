#' @title Read Accelerometry Data from GGIR Outuput
#'
#' @description Creates and returns a factory function that loads and standardizes GGIR time-series (`metashort` and `metalong`) data into an appropriate tibble. Applies timezone adjustments and identifies Non-Wear time.
#'
#' @param path_ggir_ts The explicit absolute file path to the RData file.
#' @param base Directory base string to construct the path if \code{path_ggir_ts} is not provided.
#' @param RAW Name of the raw data block to construct the path if \code{path_ggir_ts} is not provided.
#' @param start Optional start constraint (currently ignored by internal logic but kept for interface compatibility).
#' @param end Optional end constraint.
#' @param tz Timezone string. Default is \code{"Europe/Madrid"}.
#' @param ... Additional arguments.
#'
#' @return A function without arguments that, when executed, loads the specified GGIR environment and returns the formatted data frame.
#' @export
#'
#' @examples
readAccelerometryFromGGIR <-function(path_ggir_ts="",base="",RAW="",start=NA,end=NA,tz = "Europe/Madrid",...){
  function(){
    if(path_ggir_ts == "" | is.null(path_ggir_ts) | is.na(path_ggir_ts) ) path_ggir_ts=sprintf("%s/meta/basic/meta_%s.RData",base,RAW)
    
    df=NULL
    
    try({
      
      if(file.exists(path_ggir_ts)) {
        
        load(path_ggir_ts)
        
        if(!is.null(M$metashort) & !is.null(M$metalong)){
          df=M$metashort %>%
            mutate(
              timestamp = with_tz(ymd_hms(timestamp),tz = "Europe/Madrid")
            ) %>%  select (timestamp,ENMO,anglez,everything()) %>% as_tibble()
          
          
          ##NonWear
          dfNW=M$metalong  %>%
            mutate(
              timestamp = with_tz(ymd_hms(timestamp),tz = tz),
              .criterioRaw=as.integer(nonwearscore!=0),#Ponemos el primer instante como nonWear
              .criterioBout=.criterioRaw) %>%
            select(timestamp,.criterioRaw,.criterioBout) %>% as_tibble()
          
          intervalosNW=dfNW %>% criterio2Interval() %>% #Eliminando nonWear cortos de noche
            filter(! (  (difftime(to,from)<dminutes(120) & ( hour(from)>22 | hour(to)<=8)) | difftime(to,from)<dminutes(40)))
          
          df=df %>% mutate(.criterioNW= interval2criterio(df$timestamp,intervalosNW))
        }
      }
    })
    df
  }
}
