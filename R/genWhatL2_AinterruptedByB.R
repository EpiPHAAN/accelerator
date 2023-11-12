#' Title
#'
#' @param interval 
#' @param A 
#' @param B 
#' @param pctBouts 
#' @param durBoutMin 
#'
#' @return
#' @export
#'
#' @examples
genWhatL2_AinterruptedByB=function(interval,A,B,pctBouts=1,durBoutMin=dseconds(5)){
  start=interval$isOn[["from"]]
  end=interval$isOn[["to"]]
  timestamp=seq(start,end,by=5)
  timezone=tz(start)
  
  buscar     =interval2criterio(timestamp,interval[[A]])
  interrumpir=interval2criterio(timestamp,interval[[B]])
  
  df=data.frame(timestamp,
                .criterioRaw=buscar&!interrumpir,.interrumpir=interrumpir,.criterioNW=0)
  resultado=df %>% mutate(.criterioBout=criterioBout(.,pctBouts=pctBouts,durBoutMin=durBoutMin))  %>%
    getSummary() %>% .[["intervals"]]
  resultado
}