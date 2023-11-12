#' Title
#'
#' @param ts 
#' @param intervalos 
#'
#' @return
#' @export
#'
#' @examples
 interval2criterio=function(ts,intervalos){
  #ts=df$timestamp
  #intervalos=intervalosNW
  durEpoch=as.duration(ts[2]-ts[1])
  criterio=vector("logical",length(ts))

  if(is.null(intervalos)  | nrow(intervalos)==0) return(criterio)

  for(i in 1:nrow(intervalos)){
    iDesde = min(difftime(intervalos[["from"]][i],ts[1])/durEpoch,length(ts)-1)
    iHasta = min(difftime(intervalos[["to"]][i],ts[1])/durEpoch,length(ts)-1)
#    message(iDesde, " ",iHasta,"\n")
#    if(iDesde<=iHasta) criterio[1+iDesde:iHasta]=1 #Anterior a 28/02/2023
    if(iDesde<iHasta+1) criterio[1+(iDesde+1):iHasta]=1
  }

  criterio
}
