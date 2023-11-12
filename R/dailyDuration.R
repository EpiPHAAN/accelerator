#' Title
#'
#' @param whenList 
#' @param whatList 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dailyDuration=function(whenList,whatList,...){
  if(is.null(whenList)) {
    return(data.frame(day=today(),label="",what="",duration=0) %>% filter(FALSE))
  }
  resultadoEsperado=crossing(
      whenList %>% mutate(durRef=as.numeric(difftime(to,from,units = "secs"))) %>%
        select(day,
               label,
               durRef
               ),
      what=names(whatList))
  
  duraciones=map2_df(whatList,names(whatList),~ {
    intervalIntersectv2(whenList,.x) %>% mutate(what=.y)
  }) %>% 
    transmute(day=day,from=fromNew,to=toNew,what=what) %>% group_by(day,what) %>%
    summarise(duration=sum(as.integer(difftime(to,from,units="secs"))),.groups = 'drop') %>%
    ungroup() %>%
    arrange(what,day)

  
  resultadoEsperado %>% left_join(duraciones, by = c("day", "what")) %>% 
    mutate(duration=ifelse(is.na(duration),0,duration)) 
}

