#' @title measureInterval_01h
#' @description measureInterval_01h
#' @param intervals
#' @param first
#' @param last
#' @param offsetLabels
#' @param starts
#' @param duration
#' @return
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
