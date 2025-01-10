 #' Connect intervals that are close enough
 #' Generates a smaller dataframe of connected intervals using a data frame of given intervals and a distance that allows tow of them to be connected
 #'
 #' @param interval Initial dataframe of intervals to be connected
 #' @param distanciaNoche distance between ttwo intervals to allo the connection of both in just one.
 #' @param distanciaDia distance between ttwo intervals to allo the connection of both in just one.
 #'
 #' @return A dataframe of intervals, having less or equal rows tan the original
 #'
 #' @export
connectOverDistanceV2=function(interval,distanciaNoche=dhours(4),distanciaDia=dminutes(0), earlyMorning=c(1,9),interruption=NULL){
#  secDistance=distance/dseconds(1)
  interval=interval %>%
    mutate(lag=difftime(from,lag(to,1),units="secs"),
           lead=difftime(lead(from,1),to,units="secs"),
           closeEnough=  cercaElastico(to,lead(from,1),distanciaNoche,distanciaDia,earlyMorning)
             )

  #Si hay interrupciones, se marcan como no closeEnough
  if(!is.null(interruption) && nrow(interruption)>0){
    interval$closeEnough= interval$closeEnough & check_overlapWithInterruption(interval,interruption)
  }
  

  interval %>%
    mutate(change=!lag(closeEnough),
           change=ifelse(is.na(change),0,change),
           bout=cumsum(change) ) %>%
    group_by(bout) %>%
    summarise(from=first(from),to=last(to))
}



cercaElastico=function(to1,from2,distanciaNoche=dhours(4),distanciaDia=dminutes(30),earlyMorning=c(1,9)){
  distancia=difftime(from2,to1,units="hours")
  
  ( (hour(to1)+minute(to1)/60) >=earlyMorning[1] & (hour(from2)+minute(from2)/60) <=earlyMorning[2]  & distancia<=distanciaNoche) | distancia<=distanciaDia
  
}



check_overlapWithInterruption <- function(input, interruption) {
  # input e interruption tienen columnas: start, end (de tipo POSIXct o similar).
  # Se asume que input está ordenado por start, y también interruption.

  n <- nrow(input)
  
  if(is.null(interruption) || nrow(interruption) == 0) {
    return(rep(FALSE,n))
  }
  
  
  # Vector resultado a devolver
  overlap_result <- logical(n)
  
  # Función auxiliar para verificar si dos intervalos [Astart, Aend) y [Bstart, Bend)
  # tienen al menos un punto en común (solapamiento).
  intervals_overlap <- function(Astart, Aend, Bstart, Bend) {
    (Astart < Bend) & (Bstart < Aend)
  }
  
  # Recorremos todas las filas excepto la última,
  # porque la última no tiene "siguiente" y su hueco será tratado como FALSE.
  for (i in seq_len(n - 1)) {
    gap_start <- input$to[i]
    gap_end   <- input$from[i + 1]
    
    # Para la fila i, verificamos si (gap_start, gap_end) se solapa con
    # alguno de los intervalos en interruption.
    overlap_result[i] <- any(
      intervals_overlap(
        Astart = gap_start,
        Aend   = gap_end,
        Bstart = interruption$from,
        Bend   = interruption$to
      )
    )
  }
  
  # El último intervalo no tiene "siguiente", lo dejamos en FALSE
  overlap_result[n] <- FALSE
  
  return(overlap_result)
}
