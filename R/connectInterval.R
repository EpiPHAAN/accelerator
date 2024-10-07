#' Title
#'
#' @param interval 
#' @param distance 
#' @param distanceRel 
#'
#' @return
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
