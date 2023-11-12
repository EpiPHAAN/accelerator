#' Title
#'
#' @param df 
#' @param critAnglez 
#' @param limSup 
#' @param durBoutMin 
#'
#' @return
#' @export
#'
#' @examples
criterioZIB=function(df,critAnglez=5, limSup=30/1000, durBoutMin=dminutes(5)){
  #durEpoch=dseconds(5)
  durEpoch=as.duration(df$timestamp[2]-df$timestamp[1])
  windowSize=durBoutMin/durEpoch
  df %>%
    mutate(
      tmp_nearConstantFuture = ((caTools::runmax(anglez,k=windowSize, alg="C", align="left")-caTools::runmin(anglez,k=windowSize, alg="C", align="left"))<critAnglez) & caTools::runmin(ENMO<= limSup,k=windowSize, alg="C", align="left"),
      .criterio= caTools::runmax(tmp_nearConstantFuture,k=windowSize, alg="C", align="right")
    ) %>% .[[".criterio"]]

}

