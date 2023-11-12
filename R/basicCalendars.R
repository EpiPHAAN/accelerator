#' Title
#'
#' @param interval 
#' @param label 
#' @param first 
#' @param last 
#' @param offsetLabels 
#' @param starts 
#' @param duration 
#'
#' @return
#' @export
genWhen_24h=function(interval,label="24h",first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(23)+dminutes(59)+dseconds(59)){
  start=interval$isOn[["from"]]
  end=interval$isOn[["to"]]
  timezone=tz(start)
  
  # problematic with summer time... Probably need another When to deal with this
  from=(start-dhours(hour(start))-dminutes(minute(start))-dseconds(second(start)))+starts+ddays(0:as.integer(as_date(end)-as_date(start)))
  #
  #from=ymd(as.character(as_date(start)+0:8),tz=tz(start))+starts
  #to=ymd(as.character(as_date(start)+0:8),tz=tz(start))+starts
  #day(from[1])
  if(!first) from=from[-1]
  if(!last) from=from[- length(from)]
  numDias=length(from)-1
  
  if(numDias>0){
    data.frame(from=from) %>% mutate(
      to=from+duration,
      day=as_date(from[1]+offsetLabels)+0:numDias,
      label=label) %>%
      as_tibble()
  }
  else {
    if(numDias==0){
      data.frame(from=from) %>% mutate(
        to=from+duration,
        day=as_date(from[1]+offsetLabels),
        label=label) %>%
        as_tibble()
    } else {
      #Void/Trivial data frame
      data.frame(from=df$timestamp[1],to=df$timestamp[nrow(df)],day=as.Date(df$timestamp[1]),label=label) %>%
        as_tibble()
    }
  }
}



#' Title
#'
#' @param whenFrom 
#' @param whenTo 
#' @param label 
#' @param offset 
#'
#' @return
#' @export
#'
#' @examples
genWhen_StartFrom_EndFrom=function(whenFrom,whenTo,label="when_1_2",offset=0){
  if(is.null(whenFrom)||is.null(whenTo)) return(data.frame(from=now(),to=now(),day=today(),label=label) %>% as_tibble() %>% filter(FALSE))
  whenFrom  %>%  transmute(from=from,day=day,label=label) %>%
    left_join(whenTo %>% transmute(to=from,day=day)%>% mutate(day=day+offset),by="day") %>% 
    filter(from<to) %>% select(from,to,day)%>% mutate(label=label)
}



#' Title
#'
#' @param whenFrom 
#' @param whenTo 
#' @param label 
#' @param offset 
#'
#' @return
#' @export
#'
#' @examples
genWhen_StartTo_EndFrom=function(whenFrom,whenTo,label="when_1_2",offset=0){
  if(is.null(whenFrom)||is.null(whenTo)) return(data.frame(from=now(),to=now(),day=today(),label=label) %>% as_tibble() %>% filter(FALSE))
  whenFrom  %>%  transmute(from=to,day=day,label=label) %>%
    left_join(whenTo %>% transmute(to=from,day=day) %>% mutate(day=day+offset),by="day") %>%
    filter(from<to) %>% select(from,to,day)%>% mutate(label=label)
}



#' Title
#'
#' @param interval 
#' @param label 
#' @param first 
#' @param last 
#' @param offsetLabels 
#' @param starts 
#' @param duration 
#'
#' @return
#' @export
genWhen_daily=function(interval,label="daily",first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(23)+dminutes(59)+dseconds(59)){
  genWhen_24h(interval,label=label,first=first,last=last,offsetLabels=offsetLabels,starts=starts,duration=duration) %>%
    intervalIntersectv2(interval$isOn) %>%
    transmute(from=fromNew,to=toNew,day=day,label=label)
}


#' Title
#'
#' @param interval 
#' @param label 
#'
#' @return
#' @export
genWhen_start2end=function(interval,label="start2end"){
  start=interval$isOn[["from"]]
  end=interval$isOn[["to"]]
  timezone=tz(start)
  
  data.frame(
    from=start,
    to=end) %>% mutate(
      day=as_date(from),
      label=label) %>% as_tibble()
}




#' Title
#'
#' @param interval 
#' @param label 
#'
#' @return
#' @export
genWhen_mostQuiet=function(interval,label="mostQuiet"){
  interval$SIB2 %>% intervalReposo(
    interval$MuyQuieto,
    distance1=parametrosAcelerometria$distanceMiniSib,
    distance2=parametrosAcelerometria$distanceMiniQuiet) %>% 
    intervalBiggerOfDay(interval$SIB) %>% mutate(label=label)
}


#' Title
#'
#' @param interval 
#' @param label 
#' @param durMin 
#' @param durMax 
#'
#' @return
#' @export
genWhen_bedJB=function(interval,label="bed",durMin=dminutes(160),durMax=dhours(24)){
  mayorReposo=genWhen_mostQuiet(interval)
  SIBDormir=interval$SIB %>% intervalIntersectv2(mayorReposo) %>% select(from,to,day)
  QuietoDormir=interval$MuyQuieto %>% intervalIntersectv2(mayorReposo) %>% select(from,to,day)
  
  SIBDormir %>% intervalReposo( QuietoDormir, 
                                distance1=parametrosAcelerometria$distanceSib,
                                distance2=parametrosAcelerometria$distanceQuiet) %>%
    connectOverDistanceV2(parametrosAcelerometria$distanceSedentaryEarlyMorning,
                          parametrosAcelerometria$distanceSedentaryRestOfDay,
                          parametrosAcelerometria$earlyMorning) %>%
    intervalBiggerOfDay() %>% mutate(label=label) %>% select(from,to,day,label) %>%
    filter(difftime(to,from)<=durMax & difftime(to,from)>=durMin) %>% ungroup()
}


#' Title
#'
#' @param interval 
#' @param When 
#' @param label 
#'
#' @return
#' @export
genWhen_complementaryInternal=function(interval,When,label="complementaryInternal"){
  start=interval$isOn[["from"]]
  end=interval$isOn[["to"]]
  timezone=tz(start)
  
  When  %>%
    transmute(from2=to+dseconds(1),to2=lead(from,1)+dseconds(-1),day=day) %>%
    filter(row_number()>1 & row_number()<nrow(.) | row_number()==1) %>%
    filter(!is.na(to2)) %>% transmute(from=from2,to=to2,day=day,label=label)
}




#' Title
#'
#' @param interval 
#' @param bed 
#' @param label 
#'
#' @return
#' @export
genWhen_awake=function(interval,bed,label="awake"){
  genWhen_complementaryInternal(interval,bed,label=label)
}


#' Title
#'
#' @param interval 
#' @param When 
#' @param label 
#'
#' @return
#' @export
genWhen_complementary=function(interval,When,label="complementary"){
  start=interval$isOn[["from"]]
  end=interval$isOn[["to"]]
  timezone=tz(start)
  
  interno=genWhen_complementaryInternal(interval,When,label=label)
  
  datePre=min(When$day %>% first()-1, as.Date(start))
  datePost=When$day %>% last()
  primero=data.frame(from=start,to=When$from %>% first()-dseconds(1)) %>%
    mutate (day=datePre,label=label) %>%
    filter(from<to) 
  ultimo=data.frame(from=When$to %>% last()+dseconds(1),to=end)%>%
    mutate(day=datePost,label=label) %>% filter(from<to) 
  rbind(primero,interno,ultimo)
}

