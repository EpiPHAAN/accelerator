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
readAccelerometryFromAcclerometer <- function(path_accelerometer_ts="",baseBB="",RAW="",start=NA,end=NA,tz = "Europe/Madrid",...){
  function(){
    if(path_accelerometer_ts == "" ) path_accelerometer_ts=sprintf("%s/%s",baseBB,str_replace(RAW,"\\.bin|\\.csv|\\.gt3x","-timeSeries.csv.gz"))
    
    dfBB=NULL
    
    try({
      dfBB <- read_csv(path_acclerometer_ts,show_col_types = FALSE) %>% 
        rename(timestamp=time) %>% 
        mutate(timestamp=ymd_hms(timestamp) %>% with_tz(tz)) %>% 
        set_names(str_replace(names(.),"-","_"))
    })
    
    correctForDST(dfBB)    
  }
}
