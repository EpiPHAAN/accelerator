#' @title Generate Hourly Named Intervals
#' @description Creates a sequence of continuous hourly intervals covering the entire timespan of the provided data frame.
#' @param df A data frame representing the accelerometry data with a \code{timestamp} column.
#' @param first Logical. If TRUE, includes the first boundary interval. Default is TRUE.
#' @param last Logical. If TRUE, includes the last boundary interval. Default is TRUE.
#' @param offsetLabels An offset added to the labels (names) of the generated intervals. Default is 0 hours.
#' @param starts A duration shift applied to the start times. Default is 0 hours.
#' @param duration The duration of each generated interval block. Default is 1 hour (\code{period(1, "hours")}).
#' @return A named lubridate interval vector containing the hourly windows.
#' @export
measureInterval_01h<-function(df,first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=period(1,"hours")){
  start=first(df$timestamp)
  end=last(df$timestamp)
  timezone=tz(start)
  
  # problematic with summer time... Probably need another When to deal with this
  from=(start-dhours(hour(start))-dminutes(minute(start))-dseconds(second(start)))+starts+dhours(0:as.integer(difftime(end,start,units="hours")))
  if(!first) from=from[-1]
  if(!last) from=from[- length(from)]
  numHours=length(from)-1
  
    interval(from,from+duration) %>%
    set_names(format(from+offsetLabels, "%Y-%m-%d_%H"))
}
