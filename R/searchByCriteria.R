#' Title: searchByCriteria
#'
#' @param df 
#' @param EXPRESSION 
#' @param pctBouts 
#' @param durBoutMin 
#'
#' @return
#' @export
#'
#' @examples
searchByCriteria=function(df,EXPRESSION,pctBouts=0.8,durBoutMin=dminutes(1)) {
    thisCriteria=criteriaGenerator(EXPRESSION)
    tryCatch({
  df=df %>%
       mutate(.criterioRaw=thisCriteria(.)) %>%
       mutate(.criterioBout=criterioBout(.,pctBouts = pctBouts,durBoutMin = durBoutMin)) %>%
       criterio2Interval(durBoutMin = durBoutMin)
       interval(start=df$from,end=df$to)
}, error=function (e){
     NULL
})
}
