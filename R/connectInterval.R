#' @title Connect Clustered Time Intervals
#'
#' @description Merges consecutive or close time intervals into larger continuous bouts if the temporal gap between them is smaller than a specified absolute or relative threshold.
#'
#' @param interval A data frame containing time intervals, usually with \code{start} and \code{end} POSIXct columns.
#' @param distance A \code{lubridate::Duration} or time span object. Intervals separated by a gap smaller than this absolute distance will be connected into a single interval. Default is 0 minutes.
#' @param distanceRel A numeric value. Connects intervals if the gap is smaller than a proportion (\code{distanceRel}) of the duration of the current or following interval. Default is NULL.
#'
#' @return A data frame with the connected/merged time intervals (\code{start} and \code{end}).
#' @export
#'
#' @examples
connectInterval <- function(interval,distance=dminutes(0),distanceRel=NULL){
  interval=interval %>% interval2Dataframe()  %>% 
    mutate(lag=difftime(start,lag(end,1),units="secs"),
           lead=difftime(lead(start,1),end,units="secs"),
           closeEnough=FALSE)
  
  if(!is.null(distance)){
    interval=interval %>% 
      mutate(
        closeEnough=  closeEnough | difftime(lead(start,1),end)< distance
      )
  }
  
  
  if(!is.null(distanceRel)){
    interval=interval %>% 
      mutate(
        duration=difftime(end,start,units="secs"),
        closeEnough=  closeEnough | (
          (difftime(lead(start,1),end)< distanceRel*duration) &
            (difftime(lead(start,1),end) < distanceRel*lead(duration,1)))
      )
  }
  
  
  
  
  interval %>%
    mutate(change=!lag(closeEnough),
           change=ifelse(is.na(change),0,change),
           bout=cumsum(change) ) %>%
    group_by(bout) %>%
    summarise(start=first(start),end=last(end)) %>% 
    dataframe2Interval()
}
