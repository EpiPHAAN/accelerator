#' Indicates which rows meets the criteria for be considered part of a Bout meeting certain criteria
#'
#' Generates a vector that indicates for each row of a dataframe (usually epoch or BIN file)
#' if that row meets the criteria to be considered part of a Bout (veryfy a criteria for ca percent of time of a minimum duration),
#'
#'
#' @param df data frame with columns .criterio and eventually .criterioNW (that represents NonWear time as TRUE/FALSE).
#' @param pctBouts Numeric value between 0 and 1. Determines the minimum proportion or percentage of time within the window that the criteria must be fulfilled to be treated as a valid Bout.
#' @param durBoutMin Minimum amount of time (as a lubridate duration) that the condition must consistently be met to be considered a Bout.
#'
#' @return A boolean vector (TRUE/FALSE) indicating if the condition of belonging to a Bout is met for each corresponding epoch.
#'
#' @export
criterioBout=function(df,pctBouts=1,durBoutMin=dseconds(5)){
  #durEpoch=dseconds(5)
  durEpoch=as.duration(df$timestamp[2]-df$timestamp[1])
  windowSize=durBoutMin/durEpoch
#  if(! (df %>%assertthat::has_name(".interrupcion"))) df$.interrupcion=FALSE
  if(! ".interrupcion" %in% names (df)) df$.interrupcion=FALSE
  endrule="NA"
  df %>%
    mutate(
      .criterio=.criterioRaw & (!.interrupcion),
      from_pct = caTools::runmean(.criterio, k=windowSize, alg="C", align = "left")*(1-caTools::runmax(.interrupcion, k=windowSize, alg="C", align = "left",endrule=endrule)),
      from_stat = .criterio & from_pct >= pctBouts,
      from_tramo=caTools::runmax(from_stat ,k=windowSize,alg="C",align="right",endrule=endrule), #& .criterio,
      to_pct = caTools::runmean(.criterio, k=windowSize, alg="C", align = "right")*(1-caTools::runmax(.interrupcion, k=windowSize, alg="C", align = "right",endrule=endrule)),
      to_stat = .criterio & to_pct >= pctBouts,
      to_tramo=caTools::runmax(to_stat ,k=windowSize,alg="C",align="left",endrule=endrule), #& .criterio,
      to_from=to_tramo & from_tramo
    ) %>% .[["to_from"]]
}



