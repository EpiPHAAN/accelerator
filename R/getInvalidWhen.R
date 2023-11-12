propagateInvalidation=function(when,dfInvalid,cascadeReason,cascadeConsequence){
  invalidate=when %>% pluck(cascadeConsequence) 

  result=dfInvalid
  if(!is.null(invalidate)){
    invalidate=invalidate %>% select(day,label) %>% rename(when=label)
  result =dfInvalid %>% 
    filter(when==cascadeReason) %>% select(day,reason) %>%
    left_join(invalidate,by="day") %>%
    filter(!is.na(when)) %>% 
    mutate(reason=str_c(reason,"_",cascadeReason))
  }
  result
}


invalidaEnCascada=function(when,dfInvalid,cascadeList){
  map2_df(names(cascadeList),cascadeList,~map_df(.y,partial(propagateInvalidation,when=when,dfInvalid = dfInvalid,cascadeReason = .x)))
}


getCascadeInvalid=function(when,dfInvalid){
  resultado=dfInvalid
  state_old=resultado %>% distinct(day,when)
  repeat {
    resultado=dfInvalid %>% bind_rows(invalidaEnCascada(when,dfInvalid,currentProject$whenValidity$cascadeInvalid))
    new_state=resultado %>% distinct(day,when)
#    if(isTRUE(all.equal(new_state,state_old))) break
    if(isTRUE(all_equal(new_state,state_old))) break
    state_old=new_state
  }
  resultado
}


#' Title
#'
#' @param summaryList 
#'
#' @return
#' @export
#'
#' @examples
getInvalidWhen=function(summaryList,when=NULL){
  cat("\n Computing invalid `WHEN` directly\n")
  
  #pb <- progress_estimated(length(summaryList))
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) :eta", total = length(summaryList))
  
  getInvalidWhenWithProgress <- function(...){
    #pb$tick()$print()
    pb$tick()
    currentProject$whenValidity$directInvalidation(...)
  }
  
  result=map(summaryList,getInvalidWhenWithProgress)
  
  
  cat("\n Computing invalid 'WHEN' in cascade \n")

  if(!is.null(when) & 
     !is.null(currentProject$whenValidity$cascadeInvalid) &
     length(currentProject$whenValidity$cascadeInvalid)>0){
#    pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) :eta", total = length(when))
#    getCascadeInvalidWithProgress <- function(...){
#      #pb$tick()$print()
#     pb$tick()
#      getCascadeInvalid(...)
#    }
    
#    result=map2(when,result,getCascadeInvalidWithProgress)
    result=map2(when,result,getCascadeInvalid)
  }
  result
}




