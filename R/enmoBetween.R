#' Title
#'
#' @param df 
#' @param limInf 
#' @param limSup 
#' @param pctBouts 
#' @param durBoutMin 
#'
#' @return
#' @export
enmoBetween=function(df,limInf=40/1000,limSup=100/1000,pctBouts=0.8,durBoutMin=dminutes(1)) {
  df %>%
       mutate(.criterioRaw=criterioENMO (.,limInf=limInf,limSup=limSup)) %>%
       mutate(.criterioBout=criterioBout(.,pctBouts = pctBouts,durBoutMin = durBoutMin)) %>%
       getSummary(durBoutMin=durBoutMin)
  }
