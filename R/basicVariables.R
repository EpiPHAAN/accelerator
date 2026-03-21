
nonWear= function(df,...) {
  #' summarises Non Wear time.
  #' Criteria: GGIR. Viene de los epochs, creado en fase 1. FALTA describirlo.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  df %>% mutate(.criterioRaw=.criterioNW | criterioSIB(., critAnglez=2,durBoutMin=dminutes(120)),.criterioBout=.criterioRaw) %>%
    getSummary(maximoHorasNonWear=Inf,minimoHorasValidas = -Inf)
}


wear= function(df,...) {
  #' summarises wear time.
  #' Criteria: GGIR. Viene de los epochs, creado en fase 1. FALTA describirlo.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  df %>% mutate(.criterioRaw=!(.criterioNW | criterioSIB(., critAnglez=2,durBoutMin=dminutes(120))),.criterioBout=.criterioRaw) %>%
    getSummary(maximoHorasNonWear=Inf,minimoHorasValidas = -Inf)
}


isOn= function(df,...) {
  #' summarises time of Non Wear.
  #' Criteria: GGIR. Viene de los epochs, creado en fase 1. FALTA describir.
  #' @param df Dataframe of epochs with two columns: datetime and ENMO
  #' @return a list with a summary of the periods matching the criteria
  #' @export
  df %>% mutate(.criterioRaw=1,.criterioBout=1) %>%
    getSummary(maximoHorasNonWear=Inf,minimoHorasValidas = -Inf)
}




SIB=function(df,critAnglez = 5, durBoutMin = dminutes(5),...) {
  #' Summarises Sustained Inactivity Bouts (SIB) according to the criteria by Vincent van Hees
  #' Critera: Identify 5-min-bouts with < 5 degrees change in angle.
  #' @param df Dataframe of epochs with two columns: datetime and XXXXXX.
  #' @return a list with a summary of the periods matching the criteria.
  #' @export
  #'
  df %>%
    mutate(.criterioRaw=criterioSIB(.,critAnglez =critAnglez, durBoutMin = durBoutMin),.criterioBout=.criterioRaw) %>%
    getSummary(offset=dhours(2))
}
