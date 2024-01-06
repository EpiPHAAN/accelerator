#' Helps create functions for evaluating boolean expressions over a data frame that are going to be used as criteria for searching bouts
#'
#' @param EXPRESSION 
#'
#' @return A function that takes a data frame with accelerometry data and returns a vector of booleans
#' @export
#'
#' @examples
criteriaGenerator=function(EXPRESSION){
  expr <- rlang::parse_expr(EXPRESSION)
  function(df,useNW=TRUE){
  if(!useNW | ! (df %>%assertthat::has_name(".criterioNW"))) df$.criterioNW=FALSE

  df %>% mutate(.criterio= (!!expr & (!.criterioNW))) %>%.[[".criterio"]]
  }
}
