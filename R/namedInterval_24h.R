#' @title measureInterval_24h
#' @description measureInterval_24h
#' @param intervals
#' @param first
#' @param last
#' @param offsetLabels
#' @param starts
#' @param duration
#' @return
#' @export
measureInterval_24h<-function(df,first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=period(1,"days"),.isOn=NULL,withName=TRUE){
  if(!is.null(.isOn)) {
    start=int_start(.isOn[1])
    end=int_end(.isOn[1])
  } else {
  start=first(df$timestamp)
  end=last(df$timestamp)
  }
  
  
  # problematic with summer time... Probably need another When to deal with this
  #from=(start-dhours(hour(start))-dminutes(minute(start))-dseconds(second(start)))+starts+ddays(0:as.integer(as_date(end)-as_date(start)))
  #start=ymd_hms("2019-10-27 00:00:00",tz="Europe/Madrid")
  #end=ymd_hms("2019-10-30 00:00:00",tz="Europe/Madrid")
  inicio <- floor_date(start, unit = "day")
  fin <- ceiling_date(end, unit = "day")
  
  fechas_medianoche <- seq(from = inicio, to = fin, by = "day")
  redondeoSup=ceiling_date(fechas_medianoche, unit = "day") %>% unique()
  redondeoInf=floor_date(fechas_medianoche, unit = "day") %>% unique()
  if(length(redondeoSup)<length(redondeoInf)) fechas_medianoche=redondeoInf
  if(length(redondeoInf)<length(redondeoSup)) fechas_medianoche=redondeoSup
  
  
  from=(fechas_medianoche)+starts


  
  if(!first) from=from[-1]
  if(!last) from=from[- length(from)]
  numDias=length(from)-1
  
  empieza=from[1:numDias]
  if(duration==period(1,"days")) {termina=from[2:length(from)] 
  } else {termina = empieza+duration}
  
  
  if (withName){
    interval(empieza,termina) %>%
      set_names(format(from[1:numDias]+offsetLabels, "%Y-%m-%d"))
} else {
  interval(empieza,termina)
}
}
