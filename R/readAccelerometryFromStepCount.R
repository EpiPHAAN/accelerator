#' Title
#'
#' @param path_stepcount_ts 
#' @param baseStepCount 
#' @param RAW 
#' @param start 
#' @param end 
#' @param tz 
#' @param ... 
#'
#' @returns
#' @export
#'
#' @examples
readAccelerometryFromStepCount <- function(path_stepcount_ts="",baseStepCount="",RAW="",start=NA,end=NA,tz = "Europe/Madrid",...){
  function(){
    if(path_stepcount_ts == "" ) stop("No hay path_stepcount_ts")       
    #path_biobank_ts=sprintf("%s/%s",baseStepCount,str_replace(RAW,"\\.bin|\\.csv","-timeSeries.csv.gz"))
    
    dfStepCount=NULL
    
    try({
      dfStepCount <- read_csv(path_stepcount_ts,show_col_types = FALSE) %>% 
        filter(complete.cases(.)) %>% 
        rename(timestamp=time) %>% 
        mutate(timestamp=force_tz(timestamp,tz)) %>% 
        mutate(.nextEpochMissing=as.integer(difftime(lead(timestamp),timestamp))>120,
               Steps=ifelse(.nextEpochMissing,0,Steps),
               steps_acc=cumsum(Steps), 
               cadence=Steps/as.integer(difftime(timestamp,lag(timestamp)))) %>% 
        select(timestamp,cadence,steps_acc) %>% 
        filter(complete.cases(.))
      
    })
    correctForDST(dfStepCount)
  }
}
