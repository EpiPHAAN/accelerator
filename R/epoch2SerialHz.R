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
