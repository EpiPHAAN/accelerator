#' Title
#'
#' @param When 
#' @param What 
#' @param by 
#'
#' @return
#' @export
#'
#' @examples
summariseWhatByWhen=function(When, What, by="day"){
  if(is.null(When)|is.null(What)) return(NULL)
  

  What = When %>% intervalIntersectv2(What) %>%
    mutate(from=fromNew,to=toNew) %>%
    select(from,to,everything(),-fromNew,-toNew)
  
  tableRef=When  %>% mutate(duration=difftime(to,from,units="secs"))       %>%
    group_by_at(by,.groups="drop")  %>%
    summarise(durationRef=as.integer(sum(duration,na.rm=T)))
  
  if(nrow(When)>0) timezone=tz(When$from[1]) else timezone="Europe/Madrid"
  
  tableWhat=What       %>%
    mutate(
      duration=difftime(to,from,units="secs"),
      fecha_datetime= ymd_h(str_c(as.character(day)," 00"),tz=timezone),
      hfrom=round(as.numeric(difftime(from,fecha_datetime,units="hours")),4),
      hto=round(as.numeric(difftime(to,fecha_datetime,units="hours")),4),
    ) 
  
  if(nrow(tableWhat)==0){
    tableWhat=tableWhat %>% mutate(maxdur=0,num=0,duration=0,hfrom=NA,hto=NA)
  } else{
  tableWhat <- tableWhat %>%
    group_by_at(by,.groups="drop") %>%
    summarise(maxdur=max(duration),
              num=n(),
              duration=as.integer(sum(duration,na.rm=T)),
              hfrom=first(hfrom),
              hto=last(hto)
    )
  }
 
  tableRef %>% left_join(tableWhat,by=by) %>%
    ungroup() %>%
    mutate(
      duration=ifelse(is.na(duration),0,duration),
      num=ifelse(is.na(num),0,num),
      maxdur=ifelse(is.na(maxdur),0,maxdur))

  
}