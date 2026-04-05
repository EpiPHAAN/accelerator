#' @title Generate Daily Named Intervals
#' @description Creates a sequence of continuous 24-hour intervals covering the timespan of the provided data frame, aligned with midnight.
#' @param df A data frame representing the accelerometry data with a \code{timestamp} column.
#' @param first Logical. If TRUE, includes the first boundary interval. Default is TRUE.
#' @param last Logical. If TRUE, includes the last boundary interval. Default is TRUE.
#' @param offsetLabels An offset added to the daily labels (names). Default is 0 hours.
#' @param starts A duration offset applied to the start times. Default is 0 hours.
#' @param duration The duration of each generated interval block. Default is 1 day (\code{period(1, "days")}).
#' @param .isOn Optional explicit interval bounds to delimit the sequence generation.
#' @param withName Logical. If TRUE, assigns formatted date string names to each interval. Default is TRUE.
#' @return A named (or unnamed) lubridate interval vector containing the daily windows.
#' @export
measureInterval_24h<-function(df,first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=period(1,"days"),.isOn=NULL,withName=TRUE){
    if (!is.null(.isOn)) {
      start = int_start(.isOn[1])
      end = int_end(.isOn[1])
    }  else {
      start = first(df$timestamp)
      end = last(df$timestamp)
    }
    inicio <- floor_date(start, unit = "day")
    fin <- ceiling_date(end, unit = "day")+dhours(1)
    fechas_medianoche <- seq(from = inicio, to = fin, by = "24 hours")
    from = (fechas_medianoche) + starts
    if (!first)
      from = from[-1]
    if (!last)
      from = from[-length(from)]
    numDias = length(from) - 1
    empieza = from[1:numDias]
    if (duration == period(1, "days")) {
      termina = from[2:length(from)]
    }  else {
      termina = empieza + duration
    }
    if (withName) {
      interval(empieza, termina) %>% set_names(format(from[1:numDias] +
                                                        offsetLabels, "%Y-%m-%d"))
    }  else {
      interval(empieza, termina)
    }
  }
  