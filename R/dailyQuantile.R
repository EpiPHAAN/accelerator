#' Title
#'
#' @param whenList 
#' @param whatList 
#' @param q 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dailyQuantile=function(whenList,whatList,q=0.5,...){
  map2_df(whatList,names(whatList),
          ~ {whenList %>% intervalIntersectv2(.x) %>% transmute(from=fromNew,to=toNew,dia=day,day=day) %>%
              group_by(dia) %>%
              group_modify(partial(quantileForIntervals,q=q)) %>% mutate(q=q,hour=as.numeric(hour)) %>% ungroup() %>% transmute(day=dia,what=.y,q=q,hour=hour)}
  )
}
