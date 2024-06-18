readAccelerometryFromGGIR <-function(path_ggir_ts="",base="",RAW="",start=NA,end=NA,tz = "Europe/Madrid",...){
function(){
  if(path_ggir_ts == "" | is.null(path_ggir_ts) | is.na(path_ggir_ts) ) path_ggir_ts=sprintf("%s/meta/basic/meta_%s.RData",base,RAW)
  
  df=NULL
  
  try({
    
    if(file.exists(path_ggir_ts)) {
      
      load(path_ggir_ts)
    
      if(!is.null(M$metashort) & !is.null(M$metalong)){
        df=M$metashort %>%
          mutate(
          timestamp = with_tz(ymd_hms(timestamp),tz = "Europe/Madrid")
        ) %>%  select (timestamp,ENMO,anglez,everything()) %>% as_tibble()
        
    
    ##NonWear
    dfNW=M$metalong  %>%
      mutate(
        timestamp = with_tz(ymd_hms(timestamp),tz = tz),
        .criterioRaw=as.integer(nonwearscore!=0),#Ponemos el primer instante como nonWear
        .criterioBout=.criterioRaw) %>%
      select(timestamp,.criterioRaw,.criterioBout) %>% as_tibble()
    
    intervalosNW=dfNW %>% criterio2Interval() %>% #Eliminando nonWear cortos de noche
      filter(! (  (difftime(to,from)<dminutes(120) & ( hour(from)>22 | hour(to)<=8)) | difftime(to,from)<dminutes(40)))
    
    df=df %>% mutate(.criterioNW= interval2criterio(df$timestamp,intervalosNW))
      }
    }
  })
  df
 }
}


readAccelerometryFromGGIR3 <-function(path_ggir3_ts="",base="",RAW="",start=NA,end=NA,tz = "Europe/Madrid",...){
  function(){
    if(path_ggir3_ts == "" | is.null(path_ggir3_ts) | is.na(path_ggir3_ts) ) path_ggir3_ts=sprintf("%s/meta/basic/meta_%s.RData",base,RAW)
    
    df=NULL
    
    try({
      
      if(file.exists(path_ggir3_ts)) {
        
        load(path_ggir3_ts)
        
        if(!is.null(M$metashort) & !is.null(M$metalong)){
          df=M$metashort %>%
            mutate(
              timestamp = with_tz(ymd_hms(timestamp),tz = "Europe/Madrid")
            ) %>%  select (timestamp,ENMO,anglez,everything()) %>% as_tibble()
          
          
          ##NonWear
          dfNW=M$metalong  %>%
            mutate(
              timestamp = with_tz(ymd_hms(timestamp),tz = tz),
              .criterioRaw=as.integer(nonwearscore!=0),#Ponemos el primer instante como nonWear
              .criterioBout=.criterioRaw) %>%
            select(timestamp,.criterioRaw,.criterioBout) %>% as_tibble()
          
          intervalosNW=dfNW %>% criterio2Interval() %>% #Eliminando nonWear cortos de noche
            filter(! (  (difftime(to,from)<dminutes(120) & ( hour(from)>22 | hour(to)<=8)) | difftime(to,from)<dminutes(40)))
          
          df=df %>% mutate(.criterioNW= interval2criterio(df$timestamp,intervalosNW))
        }
      }
    })
    df
  }
}
