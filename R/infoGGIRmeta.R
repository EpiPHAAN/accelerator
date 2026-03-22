#' Title
#'
#' @param path 
#' @param pb 
#' @param tz 
#'
#' @return
#' @export
#'
#' @examples
infoGGIRmeta <- function(path,pb=NULL,tz= "Europe/Madrid"){
  #  pb <- progress_estimated(length(file_list))
  start=end=now()+dseconds(NA_integer_)
  serial=NA_character_
  hz=NA_real_
  
  try({
    load(path)
    if(!is.null(M$metashort)){
      start=ymd_hms(first(M$metashort$timestamp))
      end=ymd_hms(last(M$metashort$timestamp))
    }
    
    ###Leer serial y hz de forma independiente del modelo de acelerometro
    #    serial=as.integer(as.character(I$header["Device_Unique_Serial_Code","value"]))
    #    hz=as.numeric(as.character(I$header["Measurement_Frequency","value"]))
    serialepoch=epoch2SerialHz(I)
    serial=serialepoch$serial
    hz=serialepoch$hz
  })
  if(!is.null(pb)) pb$tick()$print()
  data.frame(start=start,end=end,serial=serial,hz=hz,stringsAsFactors = F) 
}
