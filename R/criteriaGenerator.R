#' Helps create functions for evaluating boolean expressions over a data frame that are going to be used as criteria for searching bouts.
#'
#' @param EXPRESSION A string representing an R logical expression (e.g., \code{"ENMO > 0.05"}). This expression will be evaluated dynamically across the rows of the data frame.
#' @param useNW Logical. If TRUE, the generated function will automatically enforce a non-wear condition (\code{!.criterioNW}) to ensure intervals are true wear time. Default is TRUE.
#' @return A function that takes a data frame with accelerometry data and returns a vector of booleans
#' @export
#'
#' @examples
criteriaGenerator <- function(EXPRESSION,useNW=TRUE){
  expr <- rlang::parse_expr(EXPRESSION)
  if(useNW) return(
    function(df){
      df %>% mutate(.criterio= (!!expr & (!.criterioNW))) %>%.[[".criterio"]]
    }
  )
  function(df){
    df %>% mutate(.criterio= (!!expr)) %>%.[[".criterio"]]
  }
}
# criteriaGenerator=function(EXPRESSION){
#   expr <- rlang::parse_expr(EXPRESSION)
#   function(df,useNW=TRUE){
#   if(!useNW | ! (df %>%assertthat::has_name(".criterioNW"))) df$.criterioNW=FALSE
# 
#   df %>% mutate(.criterio= (!!expr & (!.criterioNW))) %>%.[[".criterio"]]
#   }
# }
