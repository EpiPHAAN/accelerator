#' @title Generate Daily Wear and Activity Summaries
#'
#' @description Aggregates accelerometer data by day to produce summaries of wear time, non-wear time, and valid days. Computes overall averages and weighted statistics.
#'
#' @param df A data frame containing accelerometer data with a \code{timestamp} column, alongside logical criteria like \code{.criterioRaw}, \code{.criterioBout}, and \code{.criterioNW}.
#' @param offset A \code{lubridate::Duration} offset added to the timestamps to shift the definition of a "day". Default is 0.
#' @param minimoHorasValidas Minimum number of valid 'wear' hours required for a day to be considered valid. Default is 20.
#' @param maximoHorasNonWear Maximum number of allowed 'non-wear' hours per day. Default is 2.
#' @param durBoutMin Minimum duration required to compute valid intervals using \code{criterio2Interval}. Default is 5 seconds.
#'
#' @return A list containing: \code{dailyTable} (the daily summary tibble), \code{average} (simple average of sum), \code{weightedaverage} (weighted by valid wear hours), \code{totalValidHours}, and \code{intervals}.
#' @export
#'
#' @examples
getSummary=function(df,offset=dhours(0),minimoHorasValidas=20,maximoHorasNonWear=2,durBoutMin=dseconds(5)){
  firstDay=lubridate::as_date(df[["timestamp"]][1])
  df= df %>%
    mutate(
      day=lubridate::as_date(timestamp+offset),
      diasPasados=as.integer(difftime(day,firstDay,units="days"))
    )


  dailyTable=df %>% group_by(diasPasados,day) %>%
    summarise(Suma=sum(.criterioRaw & .criterioBout)*dseconds(5)/dminutes(1),
              Duracion=as.character(dminutes(Suma)),
              HorasNonWear=sum(.criterioNW)*dseconds(5)/dhours(1),
              HorasWear=n()*dseconds(5)/dhours(1)-HorasNonWear,
              Valido=HorasWear>=minimoHorasValidas & HorasNonWear < maximoHorasNonWear
    ) %>% ungroup() %>%
    mutate(dayHuman=strftime(day,format = "%a %d-%m-%Y"))

  validDays=dailyTable  %>% filter(Valido)
  totalValidHours=validDays  %>%  .[["HorasWear"]] %>% sum(na.rm=T)
  average=validDays %>% .[["Suma"]] %>% mean(na.rm=T)
  weightedaverage=validDays %>% mutate(Contribution=Suma*HorasWear) %>% .[["Contribution"]] %>% sum(na.rm=T)/totalValidHours
  list(dailyTable=dailyTable, 
       average=as.numeric(average),
       weightedaverage=as.numeric(weightedaverage),
       totalValidHours=totalValidHours,
       intervals=df %>% criterio2Interval(durBoutMin = durBoutMin))
}
