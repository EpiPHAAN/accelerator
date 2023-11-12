#' Title
#'
#' @param what 
#' @param when 
#'
#' @return
#' @export
#'
#' @examples
summaryWhatByWhen = function(what,when) {
  oldw <- getOption("warn")
  options(warn = -1)
  #if(exists("pb")) pb$tick()$print()
  
  varsToGenerate=currentProject$whenMeasuresWhat %>%
    map(~ data.frame(what=.x,stringsAsFactors = FALSE))  %>%
    list2df(label = "when") %>%
    as_tibble()
  result=map2_df(varsToGenerate[["what"]] %>% as.list(),
                 varsToGenerate[["when"]] %>% as.list(),
                 ~ {df=summariseWhatByWhen(when[[.y]],#doSummary
                                           what[[.x]])
                 if(!is.null(df)) df %>% mutate(what=.x,when=.y) else NULL
                 }
  )
  options(warn = oldw)
  result
}
