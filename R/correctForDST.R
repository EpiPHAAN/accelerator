#' @title Correct Timestamps for Daylight Saving Time (DST) Transitions
#' 
#' @description Adjusts the timestamps in a data frame to prevent gaps or overlaps caused by summer/winter time changes. It safely handles 3600-second offsets if a DST transition occurs within the dataset.
#'
#' @param dfBB A data frame containing a \code{timestamp} column (POSIXct objects).
#'
#' @return The mutated data frame with corrected `timestamp` values to guarantee a continuous time series.
#' @export
#'
#' @examples
correctForDST<- function(dfBB){
  #Correct for summer time and winter time
  if(is.null(dfBB)) return(dfBB)
  if(nrow(dfBB)<=2) return(dfBB)
  if(dst(first(dfBB$timestamp))==dst(last(dfBB$timestamp))) return(dfBB)
  #
  if(dst(dfBB$timestamp[1])){
    dfBB = dfBB %>%
      mutate(timestamp = if_else(
        !dst(timestamp),  # Condición: si no está en horario de verano (invierno)
        timestamp - dseconds(3600),  # Resta 3600 segundos
        timestamp  # Mantén los otros valores sin cambios
      ))
  } else {
    dfBB = dfBB %>%
      mutate(timestamp = if_else(
        dst(timestamp),  # Condición: si está en horario de verano (verano)
        timestamp + dseconds(3600),  # Suma 3600 segundos
        timestamp  # Mantén los otros valores sin cambios
      ))
  }
  dfBB
}