#' @title Search Bouts by Dynamic Criteria
#'
#' @description Dynamically applies a specific criterion string against a dataframe and subsequently groups consecutive positively evaluated epochs into distinct intervals (bouts) based on minimum percentage and duration thresholds.
#'
#' @param df The data frame of timeline/epoch measurements.
#' @param EXPRESSION A logical expression string (e.g., `"ENMO > 0.05"`) to be dynamically parsed.
#' @param pctBouts The required fraction (percentage from 0 to 1) of time within the window that the criteria must hold. Default is 0.8.
#' @param durBoutMin The required minimum continuous rolling duration for the bout to be confirmed. Default is 1 minute.
#' @param runmean Logical. If TRUE, applies a running mean smoothing filter globally to referenced fields before searching. Default is FALSE.
#' @param useNW Logical. If TRUE, avoids returning bouts on detected NonWear time. Default is TRUE.
#'
#' @return A list/vector of validated interval bouts (start and end times).
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
