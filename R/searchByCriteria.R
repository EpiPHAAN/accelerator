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
searchByCriteria <- function(df,EXPRESSION,pctBouts=0.8,durBoutMin=dminutes(1),runmean=FALSE,useNW=TRUE) {
  if(runmean){
    message("runmean")
    ws=as.integer(durBoutMin/(difftime(df$timestamp[2],df$timestamp[1])))
    toTransform=names(df)[str_detect(EXPRESSION,names(df))]
    for(v in toTransform){
      df[[v]]=caTools::runmean(df[[v]],ws,align="left",alg="fast",endrule="mean")
      message(str_c(sum(!is.na(df[[v]])),"/",length(df[[v]])," valid values"))
    }
  }
  
    thisCriteria=criteriaGenerator(EXPRESSION, useNW=useNW)
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
