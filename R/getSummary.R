#' Title
#'
#' @param df 
#' @param offset 
#' @param minimoHorasValidas 
#' @param maximoHorasNonWear 
#' @param durBoutMin 
#'
#' @return
#' @export
#'
#' @examples
getSummary=function(df,offset=dhours(0),minimoHorasValidas=20,maximoHorasNonWear=2,durBoutMin=dseconds(5)){
  firstDay=lubridate::as_date(df[["timestamp"]][1])
  df= df %>%
    mutate(
      day=lubridate::as_date(timestamp+offset),
      diasPasados=as.integer(difftime(day,firstDay,units="days"))
    )


  dailyTable=df %>% group_by(diasPasados,day) %>%
    summarise(Suma=sum(.criterioRaw & .criterioBout)*dseconds(5)/dminutes(1),
              Duracion=as.character(dminutes(Suma)),
              HorasNonWear=sum(.criterioNW)*dseconds(5)/dhours(1),
              HorasWear=n()*dseconds(5)/dhours(1)-HorasNonWear,
              Valido=HorasWear>=minimoHorasValidas & HorasNonWear < maximoHorasNonWear
    ) %>% ungroup() %>%
    mutate(dayHuman=strftime(day,format = "%a %d-%m-%Y"))

  validDays=dailyTable  %>% filter(Valido)
  totalValidHours=validDays  %>%  .[["HorasWear"]] %>% sum(na.rm=T)
  average=validDays %>% .[["Suma"]] %>% mean(na.rm=T)
  weightedaverage=validDays %>% mutate(Contribution=Suma*HorasWear) %>% .[["Contribution"]] %>% sum(na.rm=T)/totalValidHours
  list(dailyTable=dailyTable, 
       average=as.numeric(average),
       weightedaverage=as.numeric(weightedaverage),
       totalValidHours=totalValidHours,
       intervals=df %>% criterio2Interval(durBoutMin = durBoutMin))
}
