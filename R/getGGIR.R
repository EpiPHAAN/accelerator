#' Title
#'
#' @param base 
#' @param RAW 
#' @param start 
#' @param end 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
getGGIR=function(base,RAW,start=NA,end=NA,...){
  path_part1=sprintf("%s/meta/basic/meta_%s.RData",base,RAW)
  path_part5=sprintf("%s/meta/ms5.out/%s.RData",base,RAW)
  
  df=NULL
  dfGGIR=data.frame(from=now(),to=now(),day=today(),label="bedGGIR") %>% filter(FALSE) %>% as_tibble()
  
  try({
    
    if(file.exists(path_part1)) {
      
      load(path_part1)
    
      if(!is.null(M$metashort) & !is.null(M$metalong)){
        df=M$metashort %>%
          mutate(
          timestamp = with_tz(ymd_hms(timestamp),tz = "Europe/Madrid")
        ) %>%  select (timestamp,ENMO,anglez,everything()) %>% as_tibble()
    
    
    ##NonWear
    dfNW=M$metalong  %>%
      mutate(
        timestamp = with_tz(ymd_hms(timestamp),tz = "Europe/Madrid"),
        .criterioRaw=as.integer(nonwearscore!=0),#Ponemos el primer instante como nonWear
        .criterioBout=.criterioRaw) %>%
      select(timestamp,.criterioRaw,.criterioBout) %>% as_tibble()
    
    intervalosNW=dfNW %>% criterio2Interval() %>% #Eliminando nonWear cortos de noche
      filter(! (  (difftime(to,from)<dminutes(120) & ( hour(from)>22 | hour(to)<=8)) | difftime(to,from)<dminutes(40)))
    
    df=df %>% mutate(.criterioNW= interval2criterio(df$timestamp,intervalosNW))
      }
    }
  })
  
  
  
  
  ##Si es posible, añadimo el criterio de Cama GGIR 
  
  if(!is.null(df) ){    
    df$.criteriocamaGGIR=NA_integer_
    if(!file.exists(path_part5)){
      message("No hay fase 5 calculada para ", path_part5)}
    else {
      try({
        load(path_part5)
        

        if ("calendardate" %in% names(output)){
          #Es el formato viejo
          dfGGIR= output %>% transmute(from=fechaHora2datetime(calendardate,acc_onset),
                                       to=fechaHora2datetime(calendardate,acc_wake),
                                       day=as.Date(ymd(calendardate)),
                                       label="GGIR") %>%
            as_tibble() %>% filter(complete.cases(.)) 
        } else {
          #Es el formato nuevo
          dfGGIR= output %>% transmute(from=fechaHora2datetime(calendar_date,sleeponset),
                                       to=fechaHora2datetime(calendar_date,wakeup),
                                       day=as.Date(ymd(calendar_date)),
                                       label="GGIR") %>%
            as_tibble() %>% filter(complete.cases(.)) 
        }

        
        df=df %>% mutate(.criteriocamaGGIR=as.logical(interval2criterio(df$timestamp,dfGGIR)))
      })
    }
  } 
  
  
  #dfNuevo=df
  # Si al hacer la restricción de fechas no dejamos al bin sin datos, hacerla. Si no quedarnos al menos un dia
  # que haga de testigo
  if(!is.na(start) & !is.na(end) & difftime(end,start)>=dhours(24) & is.data.frame(df)) {
    df=df %>% filter(timestamp >= start & timestamp <=end)
  }

  if(isTRUE(is.data.frame(df) & nrow(df)>0)){
   start=df$timestamp[1]
   end=last(df$timestamp)
  }
  

  list(FASE1=df,FASE5=dfGGIR,start=start,end=end)
}
