#' Title
#'
#' @param df 
#' @param limInf 
#' @param pctBouts 
#' @param durBoutMin 
#'
#' @return
#' @export
enmoOver=function(df,limInf = 100/1000,pctBouts=0.8,durBoutMin = dminutes(1)) {
  df %>% mutate(.criterioRaw =criterioENMO(.,limInf = limInf)) %>%
    mutate(.criterioBout=criterioBout(.,pctBouts = pctBouts,durBoutMin = durBoutMin)) %>%
    getSummary(durBoutMin=durBoutMin)
}
