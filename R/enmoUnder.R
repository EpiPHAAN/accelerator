#' Title
#'
#' @param df 
#' @param limSup 
#' @param pctBouts 
#' @param durBoutMin 
#'
#' @return
#' @export
enmoUnder=function(df,limSup=40/1000,pctBouts=1,durBoutMin=dminutes(1)) {
  df %>%
    mutate(.criterioRaw= criterioENMO(.,limSup=limSup)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = pctBouts,durBoutMin = durBoutMin)) %>%
    getSummary(durBoutMin=durBoutMin)
}