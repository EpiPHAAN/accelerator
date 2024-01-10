#' @title namedInterval_24h
#' @description namedInterval_24h
#' @param intervals
#' @param first
#' @param last
#' @param offsetLabels
#' @param starts
#' @param duration
#' @return
#' @export
namedInterval_24h<-function(intervals,first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=period(1,"days")){
  start=int_start(intervals$.isOn)
  end=int_end(intervals$.isOn)
  timezone=tz(start)
  
  # problematic with summer time... Probably need another When to deal with this
  from=(start-dhours(hour(start))-dminutes(minute(start))-dseconds(second(start)))+starts+ddays(0:as.integer(as_date(end)-as_date(start)))
  if(!first) from=from[-1]
  if(!last) from=from[- length(from)]
  numDias=length(from)-1
  
  if(numDias>0){
    interval(from,from+duration#-dseconds(5)
             ) %>% set_names(as.character(as_date(from[1]+offsetLabels)+0:numDias))
      }  else {
    if(numDias==0){
      interval(from,from+duration) %>% set_names(as.character(from[1]+offsetLabels))
    } else {
      #Void
      .isOn() %>% set_names(as.character(as_date(start)))
    }
  }
}
