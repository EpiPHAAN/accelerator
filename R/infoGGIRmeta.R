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
<<<<<<< HEAD


epoch2SerialHz=function(I){
  resultado=list(serial="",hz=0)
  
  if (str_detect(I$filename,"\\.bin$")){
    resultado=list(
      serial=as.character(I$header["Device_Unique_Serial_Code","value"]),
      hz=as.numeric(as.character(I$header["Measurement_Frequency","value"]))
    )
    
  }
  
  if (str_detect(I$filename,"\\.cwa$")){
    serial=I$header["uniqueSerialCode",1][[1]]
    if(serial<0) serial = serial+65536
    resultado=list(
      serial=as.character(serial),
      hz=as.numeric(as.character(I$header["frequency","value"][[1]]))
    )
  }
  
  
  if (str_detect(I$filename,"\\.wav$")){
    serial=I$header["IART2Id",1][[1]]
    resultado=list(
      serial=as.character(serial),
      hz=as.numeric(as.character(I$header["Config-A",1][[1]]))
    )
  }
  
  
  if (str_detect(I$filename,"RAW\\.csv$")){
    serial= str_trim(as.character(I$header["Serial Number:",1][[1]]))
    hz=str_trim(as.character(I$header["First line",1][[1]])) %>% str_replace(".*\\.at\\.([0-9.]+)\\.Hz.*","\\1")
    resultado=list(
      serial=as.character(serial),
      hz=as.numeric(hz)
    )
  }
  
  resultado
}


=======
>>>>>>> 3714706dbab669a7722a62fe79b95f9fca55d2f0
