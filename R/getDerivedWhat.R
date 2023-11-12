#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
getDerivedWhat=function(df) {
  if(!is.null(currentProject$derivedWhat) & length(currentProject$derivedWhat)>0){
    newWhat=pmap( df %>% select(what,when) ,.f = currentProject$derivedWhat)
    map2(df$what,newWhat,append)
  }
  else df %>% pluck("what")
}