
#' Title
#'
#' @param df 
#' @param critAnglez 
#' @param durBoutMin 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
SIB_v2<-function(df,critAnglez = 5, durBoutMin = dminutes(5),...) {
  df2 = df %>%
    mutate(.criterioRaw=criterioSIB(.,critAnglez =critAnglez, durBoutMin = durBoutMin),.criterioBout=.criterioRaw) %>%
    criterio2Interval(durBoutMin = durBoutMin) 
  interval(start=df2$from,end=df2$to)
}

SIB2_v2<- function(df,...) SIB_v2(df,critAnglez=10, durBoutMin=dminutes(1))

MuyQuieto_v2 <- function(df,...) {
  df2 = df %>% mutate(.criterioRaw = criterioENMO(.,limSup = 12/1000,useNW=FALSE)) %>%
    mutate(.criterioBout = criterioBout(.,pctBouts = 0.95,durBoutMin = dminutes(20))) %>% 
    criterio2Interval(durBoutMin = dminutes(20))
  interval(start=df2$from,end=df2$to)
}








# Esta es una definición de cama alternativa a las que ya existen para considerar en el estudio de comparaciones
measureInterval_bed=function(df,SIB,...){
  parametersForBed=list(
    distanceMiniSib=dminutes(7),
    distanceMiniQuiet=dminutes(10),
    distanceSib=dminutes(40),
    distanceQuiet=dminutes(5),
    earlyMorning=c(1.0,9.0),
    distanceSedentaryEarlyMorning=dhours(4),
    distanceSedentaryRestOfDay=dminutes(10)
  )
  
  label="bed"
  durMin=dminutes(160)
  durMax=dhours(24)
  intervalSIB = SIB %>% interval2Dataframe() %>% rename(from=start,to=end)
  intervalSIB2=SIB2_v2(df) %>% interval2Dataframe() %>% rename(from=start,to=end)
  intervalMuyQuieto=MuyQuieto_v2(df) %>% interval2Dataframe() %>% rename(from=start,to=end)
  mayorReposo=  intervalSIB2 %>% intervalReposo(
    intervalMuyQuieto,
    distance1=parametersForBed$distanceMiniSib,
    distance2=parametersForBed$distanceMiniQuiet) %>% 
    intervalBiggerOfDay(intervalSIB)
  
  SIBDormir=intervalSIB %>% intervalIntersectv2(mayorReposo) %>% select(from,to,day)
  QuietoDormir=intervalMuyQuieto %>% intervalIntersectv2(mayorReposo) %>%
    select(from,to,day)
  
  dfCama = SIBDormir %>% intervalReposo( QuietoDormir, 
                                         distance1=parametersForBed$distanceSib,
                                         distance2=parametersForBed$distanceQuiet) %>%
    connectOverDistanceV2(parametersForBed$distanceSedentaryEarlyMorning,
                          parametersForBed$distanceSedentaryRestOfDay,
                          parametersForBed$earlyMorning) %>%
    intervalBiggerOfDay()  %>% select(from,to,day) %>%
    filter(difftime(to,from)<=durMax & difftime(to,from)>=durMin) %>%
    ungroup() #%>% slice(-1)
  
  interval(start=dfCama$from,end=dfCama$to) %>% set_names(dfCama$day) 
}


# measureInterval_complementaryInternal mide los intervalos complementarios a los que se les pase.
# Por ejemplo el complementario de estar dormido es estar despierto.
measureInterval_complementaryInternal=function(df,interval,...){
  if(is.null(interval)) return(NULL)
  start=df$timestamp[1]
  end=df$timestamp[length(df$timestamp)]
  timezone=tz(start)
  
  dfComp=interval %>% interval2Dataframe()  %>%
    transmute(start2=end+dseconds(1),end2=lead(start,1)+dseconds(-1),name=name) %>%
    filter(row_number()>1 & row_number()<nrow(.) | row_number()==1) %>%
    filter(!is.na(end2)) %>% transmute(start=start2,end=end2,name=name)
  
  interval(start=dfComp$start,end=dfComp$end) %>% set_names(dfComp$name)
}








#' Title searchMaxActivity busca dentro de un intervalo de medida el tramo en que ocurre la mayor cantidad de cierta variable de interés 
#(ENMO, steps, etc.) en un tiempo determinado.
#'
#' @param df 
#' @param intervals 
#' @param durInterval 
#' @param colActivity 
#'
#' @return
#' @export
#'
#' @examples
searchMaxActivity=function(df,
                           intervals,
                           durInterval=dminutes(60),
                           colActivity="step_count"){
  lista=intervals %>% 
    map(~searchMaxActivity_1interval(df,interval=.x, durInterval=durInterval, colActivity=colActivity ) )
  resultado=do.call(c,lista)
  resultado
}



# Vamos a definir una nueva función: intervalo de duración dada que acumula mayor actividad acumulada en ENMO, 
# pasos o lo que se pida entre dos horas determinadas.
# Esta es una función auxiliar para la que un usuario necesitaría normalmente que es "searchMaxActivity"
searchMaxActivity_1interval=function(df, interval,durInterval=dminutes(5),colActivity="step_count",...){
  
  expr <- rlang::parse_expr(colActivity)
  start=int_start(interval)
  end=int_end(interval)
  durEpoch=difftime(df$timestamp[2],df$timestamp[1],units="secs")
  numEpochs=durInterval/durEpoch
  
  dh=df %>% filter((timestamp<=end +durInterval) & (timestamp>=start)) %>% 
    mutate(.acum=cumsum(.data[[colActivity]])) %>% 
    mutate(.diff=lead(.acum,numEpochs,default=0)-.acum)
  
  #filtrar donde se alcanza el máximo de diff
  dMax=dh %>%   filter(.diff==max(.diff)) %>% slice(1)
  
  dfRes=dh %>% filter(timestamp>=dMax$timestamp, timestamp<=dMax$timestamp+durInterval) %>% 
    filter(.acum>lag(.acum)|row_number()==1) %>% filter(!!expr>0) 
  
  interval(start=dfRes$timestamp[1],end=dfRes$timestamp[nrow(dfRes)])
}




# Funciones de agregados de nuvel uno y nivel 2 que se han descrito como interesantes.
# Se pueden añadir las que se necesiton sabiendo que van por parejas. Sea lo que sea
# que tengamos interer en clcular en nivel 1, la de nivel 2 agrega de nuevo su resultado,
# así que debe haber compatibilidad en las columnas que se usan en la agregación.



#

#' Title
#'
#' @param df 
#' @param units 
#' @param ... 
#'
#' @return Devuelve las columna: name, dur
#' @export
#'
#' @examples
aggregate_Duration<-function(df,units="secs",...){
  df %>% summarise(dur=sum(as.numeric(difftime(end,start,units=units))), .by=name)
}

#Devuelve: 
#dur : media de las duraciones
aggregate2_Duration_mean<-function(df,...){
  if(! ".valid" %in% names(df)) {df=df %>% mutate(.valid=TRUE)}
  df %>% filter(.valid) %>% summarise(dur=mean(dur))
}


#' Title
#'
#' @param df 
#' @param ... 
#'
#' @return mean_dur : media de las duraciones, sd_dur: desviación estándar de las duraciones 
#' @export
#'
#' @examples
aggregate2_Duration_meansd<-function(df,...){
  if(! ".valid" %in% names(df)) {df=df %>% mutate(.valid=TRUE)}
  df %>% filter(.valid) %>% summarise(mean_dur=mean(dur),sd_dur=sd(dur))
}





#----------------------------------------------------------------------------
#Propósito: Cuando estudiamos actividad en interesan la duración, los pasos, el ritmo y la intensidad
#Devuelve las columnas: name, steps, dur, cad,  ENMO
#' Title
#'
#' @param df 
#' @param driver 
#' @param units 
#' @param ... 
#'
#' @return name, steps, dur, cad,  ENMO
#' @export
#'
#' @examples
aggregate_DurStepCadENMO<-function(df,driver,units="secs",...){
  if(! "name" %in% names(df)) {df=df %>% mutate(name=".")}
  dfAccelerometry<- driver()
  dfAccelerometry %>%
    left_join(df,join_by(between(timestamp,start,end,bounds="[]"))) %>%
    filter(!is.na(start),!is.na(end)) %>% 
    summarise(
      steps=last(steps_acc)-first(steps_acc),
      ENMO=round(mean(ENMO),4),.by=c("name")) %>% 
    inner_join(  df %>% summarise(
      dur=sum(as.numeric(difftime(end,start,units=units))), .by=name),
      by = join_by(name)) %>% 
    mutate(cad=round(steps/dur,3)) %>% 
    select(name,steps,dur,cad,ENMO)
}

#' Title
#'
#' @param df 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregate2_DurStepCadENMO<-function(df,...){
  if(! ".valid" %in% names(df)) {df=df %>% mutate(.valid=TRUE)}
  df %>% filter(.valid) %>% summarise(mean_dur=mean(dur),
                                      mean_steps=round(mean(steps),3),
                                      mean_cad=round(mean(cad),3),
                                      max_cad=max(cad),
                                      mean_ENMO=mean(ENMO))
}



#----------------------------------------------------------------------------
#Propósito: Cuando estudiamos actividad en interesan la duración los pasos, el ritmo, la intensidad y los counts
#Devuelve las columnas: name, steps, dur, cad ENMO, count
#' Title
#'
#' @param df 
#' @param driver 
#' @param units 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregate_ENMOCountStepCad<-function(df,driver,units="secs",...){
  if(! "name" %in% names(df)) {df=df %>% mutate(name=".")}
  dfAccelerometry<- driver()
  dfAccelerometry %>%
    left_join(df,join_by(between(timestamp,start,end,bounds="[]"))) %>%
    filter(!is.na(start),!is.na(end)) %>% 
    summarise(
      steps=last(steps_acc)-first(steps_acc),
      ENMO=round(mean(ENMO),4),
      count=round(mean(BrondCount),4),
      .by=c("name")) %>% 
    inner_join(  df %>% summarise(
      dur=sum(as.numeric(difftime(end,start,units=units))), .by=name),
      by = join_by(name)) %>% 
    mutate(cad=round(steps/dur,3)) %>% 
    select(name,ENMO,count,steps,cad)
}

#Devuelve:
#max_ENMO
#max_count
#max_steps
#max_cad
#' Title
#'
#' @param df 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregate2_ENMOCountStepCad_max<-function(df,...){
  if(! ".valid" %in% names(df)) {df=df %>% mutate(.valid=TRUE)}
  df %>% filter(.valid) %>% summarise( max_ENMO=max(ENMO),
                                       max_count=max(count),
                                       max_steps=max(steps),
                                       max_cad=max(cad))
}







#----------------------------------------------------------------------------
#Propósito: Cuando estudiamos actividad en interesan la intensidad, los pasos y la cadencia, pero no la duración
#Devuelve las columnas: name, steps,  cad, ENMO
#' Title
#'
#' @param df 
#' @param driver 
#' @param units 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregate_ENMOStepCad<-function(df,driver,units="secs",...){
  if(! "name" %in% names(df)) {df=df %>% mutate(name=".")}
  dfAccelerometry<- driver()
  dfAccelerometry %>%
    left_join(df,join_by(between(timestamp,start,end,bounds="[]"))) %>%
    filter(!is.na(start),!is.na(end)) %>% 
    summarise(
      steps=last(steps_acc)-first(steps_acc),
      ENMO=round(mean(ENMO),4),
      .by=c("name")) %>% 
    inner_join(  df %>% summarise(
      dur=sum(as.numeric(difftime(end,start,units=units))), .by=name),
      by = join_by(name)) %>% 
    mutate(cad=round(steps/dur,3)) %>% 
    select(name,ENMO,steps,cad)
}

# Devuelve:
#max_ENMO
#max_steps
#max_cad
aggregate2_ENMOStepCad_max<-function(df,...){
  if(! ".valid" %in% names(df)) {df=df %>% mutate(.valid=TRUE)}
  df %>% filter(.valid) %>% summarise( max_ENMO=max(ENMO),
                                       max_steps=max(steps),
                                       max_cad=max(cad))
}















#' Title
#'
#' @param df 
#' @param wide 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregate_Duration_meanByHour<-function(df,wide=FALSE,...){
  if(! ".valid" %in% names(df)) {df=df %>% mutate(.valid=TRUE)}
  result= df %>% filter(.valid) %>% 
    mutate(hour=str_replace_all(name,".*_([0-9][0-9])$","\\1")) %>% 
    summarise(dur=mean(dur,na.rm=T), .by=hour)
  if(wide) result %>% spread(hour,dur) %>% set_names(str_c("dur_h",names(.)))
  else result
}


#' Title
#'
#' @param dfCruce 
#' @param dfName 
#' @param units 
#' @param .by 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregate_DurationRel<-function(dfCruce,dfName,units="secs",.by="name",...){
  if(! "name" %in% names(dfCruce)) {dfCruce=dfCruce %>% mutate(name=".")}
  dfName %>% 
    summarise(durRef=sum(as.numeric(difftime(end,start,units=units))), .by=name) %>% 
    inner_join(
      dfCruce %>% summarise(dur=sum(as.numeric(difftime(end,start,units=units))), .by=.by),
      by = join_by(name)) %>% mutate(durRel=dur/durRef)
}



#----------------------------------------------------------------------------
#Propósito: Estudiar horario de interés en la que ocurren actividades.
# La hora de inicio corresponde a prob=0, el final=prob=1, y se puede elegir añadir cualquier valor intermedio
#Devuelve las columnas: name, timeqxx, donde xx representa el percentil de dos dígitos de la hora
# ejemplo: timeq25=Hora en la que ha ocurrido el 25% de la actividad de interés


#' Title
#'
#' @param df 
#' @param probs 
#' @param .by 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregate_Quantile <- function(df,probs=c(0,0.25,0.5,0.75,1),.by="name",...){
  if(! "name" %in% names(df)) {df=df %>% mutate(name=".")}
  df  %>% nest(data=-c(name)) %>% 
    mutate(quantiles=map(data,intervalQuantile2,probs=probs)) %>%
    unnest(quantiles) %>% 
    select(-data) %>%     
    mutate(across(where(is.POSIXct),datetime2hour))
}


#' Title
#'
#' @param df 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregate2_Quantile<-function(df,...){
  if(! ".valid" %in% names(df)) {df=df %>% mutate(.valid=TRUE)}
  df %>% filter(.valid) %>% 
    summarise(across(starts_with("timeq"),\(x) mean(x, na.rm = TRUE)))
}

#Para que funcionen las funciones de agregado anteriores es necesario introducir en la librería
# estás otras dos funciones que serán privadas.
#' Title
#'
#' @param dfIntervalos 
#' @param probs 
#'
#' @return
#' @export
#'
#' @examples
intervalQuantile2<- function(dfIntervalos,probs=c(0,0.25,0.5,0.75,1)){
  accelerator::intervalsQuantiles(dfIntervalos %>%
                                    select(from=start,to=end), probs = probs)%>%
    select(probs,quantile) %>%
    mutate(probs=sprintf("timeq%02d",as.integer(100*probs))) %>% 
    pivot_wider(names_from = probs,values_from = quantile)
}

datetime2hour<-Vectorize(function(instante){
  fecha_medianoche <- as.POSIXct(format(instante[1], "%Y-%m-%d"))
  horas_desde_medianoche <- as.numeric(difftime(instante[1], fecha_medianoche, units = "hours"))
  if(horas_desde_medianoche>12) horas_desde_medianoche-24 else horas_desde_medianoche
})



##############################################################################




#' Title
#'
#' @param lista 
#'
#' @return
#' @export
#'
#' @examples
listaDF2DF1<-function(lista){
  lista=lista %>% keep(negate(is.null))
  if(length(lista)==0) return(data.frame())
  map2(lista,names(lista), 
       ~ {toRename=names(.x)!="name" & !str_detect(names(.x),str_c("_",.y,"$"))
       names(.x)[toRename]= str_c(names(.x)[toRename],"_",.y)
       .x}) %>%
    keep(~!is.data.frame(.x) | nrow(.x)>0) %>% 
    reduce(cbind)
}





############################################################################################################################
# aggregateVariablesLong es la función propuesta para calculos agregados tal como se describen en el fichero de configuración
# Es la única que requiere ser pública. 
# Requiere de tres funciones privadas que desglosan los cálculos fInterna1, fInterna2, fInterna3
#' Title
#'
#' @param intervals 
#' @param variablesLong 
#' @param driver 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
aggregateVariablesLong <- function(intervals, variablesLong,driver,...){
  message("aggregateVariablesLong: Entrada")
  result=variablesLong %>% map(\(.x) fInterna3(intervals,.x[["intervals"]],.x[["measureIntervals"]], .x[["variables"]], driver))
  message("aggregateVariablesLong: Salida")
  result
}


#Otro nombres más corto para el agregado de nivel 1 usado en la app.
# Tal vez deberíamos llamar a estas funciones aggregate_L1 y aggregate_L2 para que sea más claro
# qué nivel de agregado hacen

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
Agrega1<-function(...){
  aggregateVariablesLong(...)}



#Agregado de nivel 2
#' Title
#'
#' @param Agregados1 
#' @param variablesLong 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
Agrega2 <- function(Agregados1,variablesLong,...){ map2(Agregados1,
                                                        variablesLong, 
                                                        function(x,y) {
                                                          map(x, ~(.x %>% map(purrr::possibly(y$summary,otherwise=data.frame()))))
                                                        })
}





fInterna1 <- function(intervals,intervalsTxt,measureIntervalTxt,variables,driver){
  message("\nfInterna1:", intervalsTxt,"/",measureIntervalTxt)
  if(is.null(intervals[[intervalsTxt]]) | is.null(intervals[[measureIntervalTxt]])) return(data.frame())
  resultado=intervals[[intervalsTxt]] %>% intersectIntervals(intervals[[measureIntervalTxt]], useNames=TRUE,short = FALSE) %>% interval2Dataframe() %>% (variables)(driver=driver)#driver se usa por si hay no basta con los intervalos y hay que recurrir a datos crudos como pasos, enmo, que ocurren dentro de ellos.     
  resultado
}


fInterna2 <- function(intervals,intervalsTxt,measureIntervalTxt,variables,driver){
  #  message("fInterna2")
  resultado=map(intervalsTxt %>% set_names(intervalsTxt),\(.x) fInterna1(intervals,.x,measureIntervalTxt,variables,driver))
  resultado
} 

fInterna3 <- function(intervals,intervalsTxt,measureIntervalsTxt,variables,driver){
  #  message("fInterna3")
  measureIntervalsTxt=measureIntervalsTxt %>% keep(~(!is.null(intervals[[.x]]) & length(intervals[[.x]])>=1))
  map(measureIntervalsTxt %>% set_names(measureIntervalsTxt),
      \(.x) fInterna2(intervals,intervalsTxt,.x,variables,driver))}





###########################################################################################
#reduceListDf2Df es una función que transforma una lista de dataframes resultados de agregar
# en un solo dataframe, teniendo cuidado con el renombrado de las columnas y evitando duplicados
# Es una función que debería ser privada.
#' Title
#'
#' @param lista 
#'
#' @return
#' @export
#'
#' @examples
reduceListDf2Df<- function(lista){
  lista=lista %>% keep(negate(is.null)) %>% keep(is.data.frame) %>% keep(~nrow(.x)>0)
  if(length(lista)==0){
    message("No hay nada en reduceListDf2Df!!")
    return(data.frame())
  }
  resultado=map2(lista,names(lista), 
                 #No todas las columnas se renombran
                 ~ {toRename=names(.x)!="name" & !str_detect(names(.x),str_c("_",.y,"$"))
                 names(.x)[toRename]= str_c(names(.x)[toRename],"_",.y)
                 .x}) 
  resultado= resultado %>% reduce(full_join,by=join_by(name))
  resultado
}

reduceListDf2Df_columnbind<- function(lista){
  lista=lista %>% keep(negate(is.null)) %>% keep(is.data.frame) %>% keep(~nrow(.x)>0)
  if(length(lista)==0){
    message("No hay nada en reduceListDf2Df!!")
    return(data.frame())
  }
  resultado=map2(lista,names(lista), 
                 #No todas las columnas se renombran
                 ~ {toRename=names(.x)!="name" & !str_detect(names(.x),str_c("_",.y,"$"))
                 names(.x)[toRename]= str_c(names(.x)[toRename],"_",.y)
                 .x}) 
  resultado= resultado %>% reduce(cbind)
  resultado
}








###########################################################################################
# dygraphTS es la función que genera el gráfico de serie temporal en la aplicación del TFG,
# pero que creo que debería ser de utilidad en la próxima versión de la librería ya que se podría
# utilizar en futuras aplicaciones donde los usuarios/pacientes puedan recibir un informe detallado
# de su actividad.
# Requiere de funciones auxiliares privadas.
#' Title
#'
#' @param df 
#' @param intervals 
#' @param UI_ejeY 
#' @param runmeanType 
#' @param runmeanTime 
#' @param definePalette 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dygraphTS <- function(df,intervals,UI_ejeY="ENMO",runmeanType=NULL,runmeanTime=0,definePalette=NULL,...){
  UI_ejeY=UI_ejeY[1:min(2,length(UI_ejeY))]
  for(v in UI_ejeY)  df[[v]]=abs(df[[v]])
  
  durEpoch=as.integer(difftime(df$timestamp[[2]],df$timestamp[[1]],units="secs"))
  vectorColores=interval2Color(c(names(intervals)),definePalette)
  vectorColores[UI_ejeY]=c("#A0A0A066","#A0A00066")[1:length(UI_ejeY)]
  
  
  if(!is.null(runmeanType) & runmeanTime>=durEpoch){
    align=case_when(runmeanType=="Forward" ~ "right",
                    runmeanType=="Backward" ~ "left",
                    TRUE ~ "center")
    
    
    if("Forward" %in% runmeanType) {
      df[["movAvgF"]]=caTools::runmean(x=df[[UI_ejeY]], 
                                       k=as.integer(runmeanTime/durEpoch),
                                       alg="fast",
                                       align="left")
    }
    if("Backward" %in% runmeanType) {
      df[["movAvgB"]]=caTools::runmean(x=df[[UI_ejeY]], 
                                       k=as.integer(runmeanTime/durEpoch),
                                       alg="fast",
                                       align="right")
    }
    if("Center" %in% runmeanType) {
      df[["movAvgC"]]=caTools::runmean(x=df[[UI_ejeY]], 
                                       k=as.integer(runmeanTime/durEpoch),
                                       alg="fast",
                                       align="center")
    }
    
    vectorColores["movAvgF"]="#9090DDaa"
    vectorColores["movAvgC"]="#909090aa"
    vectorColores["movAvgB"]="#DD9090aa"
  }
  
  nombresIntervalosParaMostrar=names(vectorColores) %>% keep(~!str_detect(.,"^\\."))
  message(paste0(nombresIntervalosParaMostrar,sep=" "))
  rangeY=df[[UI_ejeY[1]]] %>% quantile(c(0,1),na.rm=T) %>% as.vector()
  rangeY=c(rangeY[1]*0.9+0.1*rangeY[2], rangeY[1]*0.1+0.9*rangeY[2])
  
  height=seq(rangeY[1], rangeY[2], length.out = length(nombresIntervalosParaMostrar)) %>% set_names(nombresIntervalosParaMostrar)
  
  for(interval_name in nombresIntervalosParaMostrar %>% intersect(names(intervals))){
    interval_df=intervals[[interval_name]] %>% interval2Dataframe() %>% transmute(from=start,to=end)
    try({
      interval_values=interval2criterio(df$timestamp,interval_df)*height[interval_name]
      interval_values[interval_values==0]=NaN
      if(any(interval_values>0,na.rm=TRUE)){# & sum(interval_values>0,na.rm=TRUE)< length(interval_values)*0.85){
        df[[interval_name]]=interval_values
      }
    }, silent = TRUE)
  }
  
  nombresIntervalosParaMostrar=c(nombresIntervalosParaMostrar,UI_ejeY,"movAvgB","movAvgC","movAvgF") %>% intersect(names(df))
  
  
  
  dfTS <- xts(df %>% select(all_of(nombresIntervalosParaMostrar)),order.by = df$timestamp)
  
  laPaleta=vectorColores[names(vectorColores) %in% nombresIntervalosParaMostrar] %>% set_names(NULL)
  
  
  graficoTS <- dygraph(dfTS) %>%
    dyRangeSelector() %>%  # 
    dyRoller(rollPeriod = 1) %>%
    dyAxis("y", label = UI_ejeY[1],
           valueRange = c(0,rangeY[2]*1.1),
    ) %>%
    dyOptions(colors=laPaleta,drawGrid = FALSE)
  
  if(length(UI_ejeY)==2){
    graficoTS=graficoTS %>% dySeries(UI_ejeY[2], axis = "y2")
  }
  
  graficoTS
}

interval2Color_aux = function(interval,definePalette) {
  if (interval %in% names(definePalette)) {
    return(definePalette[interval])
  } else{
    return(NA_character_)
  }
}



#' Title
#'
#' @param intervals 
#' @param definePalette 
#'
#' @return
#' @export
#'
#' @examples
interval2Color <- function(intervals,definePalette) {
  intervals = c(names(definePalette) %>% intersect(intervals),
                setdiff(intervals, names(definePalette)))
  resultado = intervals %>% set_names(intervals)
  
  resultado = map_chr(intervals, interval2Color_aux %>% partial(definePalette=definePalette)) %>% set_names(intervals)
  sinColores = sum(is.na(resultado))
  if (sinColores > 0) {
    resultado[is.na(resultado)] = colorRampPalette(c("#DDDDDD", "#999999"))(sinColores)
    
  }
  resultado
}





###########################################################################################
# girafePlot es la función que genera el gráfico de intervalos en la aplicación del TFG,
# pero que creo que debería ser de utilidad en la próxima versión de la librería ya que se podría
# utilizar en futuras aplicaciones donde los usuarios/pacientes puedan recibir un informe general
# de su actividad.
#' Title
#'
#' @param intervals 
#' @param WhenName 
#' @param intervalToPlot 
#' @param definePalette 
#' @param offLimits 
#'
#' @return
#' @export
#'
#' @examples
girafePlot <- function(intervals,WhenName=NA_character_, intervalToPlot=NULL,definePalette=NULL,offLimits=dseconds(0)){
  if(is.na(WhenName)) {
    When=measureInterval_24h(df=NULL,.isOn=intervals[[".isOn"]]) 
  } else {
    When =intervals[[WhenName]]
  }
  
  
  
  if(is.null(intervalToPlot)){
    intervalToPlot=names(intervals)
  } else {
    intervalToPlot=intersect(c(WhenName,intervalToPlot),names(intervals))
  }
  
  
  
  
  vectorColores=interval2Color(names(intervals),definePalette)
  nombresIntervalosParaMostrar=names(vectorColores) %>%
    keep(~!str_detect(.,"^\\.")) %>%
    intersect(intervalToPlot)
  vectorColores=vectorColores[nombresIntervalosParaMostrar]
  
  
  dfCruce=intervals[nombresIntervalosParaMostrar] %>%
    map2_df(
      #names(intervals[nombresIntervalosParaMostrar]
      nombresIntervalosParaMostrar,
      function(.x,.y){
        What=.x
        Cruce=intersectIntervals(What,When,useNames = TRUE,short = TRUE) 
        Cruce %>% interval2Dataframe() %>% mutate(what=.y) 
      })%>% 
    mutate(name=str_replace(name,"\\.+[0-9]+$","")) %>% 
    mutate(data_id=str_c(what,": ",format(start, "%a %d, %H:%M:%S")," - ",format(end, "%H:%M:%S")),
           tooltip=data_id) 
  
  #  dfCruce %>% filter(what=="awake") %>% group_by(name) %>% summarise_all(first)
  
  #Cambiamos el dataframe para que tenga la información relevante para el gráfico
  
  dfMeasureInterval=When %>% interval2Dataframe()
  
  cambioHora=dst(first(dfMeasureInterval$start))!=dst(last(dfMeasureInterval$end))
  if(cambioHora) { desplazar=dhours(24*7*4)
  dfCruce$start=dfCruce$start-desplazar
  dfCruce$end=dfCruce$end-desplazar
  } else { desplazar=dhours(0)}
  
  
  
  
  dfMeasureInterval=When %>% interval2Dataframe() %>% 
    mutate(las00=floor_date(ymd(name,tz=tz(start))-desplazar+dhours(3),unit="days")) %>% 
    transmute(name,startRef=las00)
  
  time00=dfMeasureInterval$startRef[1]
  
  dfMeasureInterval=dfMeasureInterval %>% 
    mutate(trasladar=dhours(round(as.numeric(difftime(startRef,time00,units ="hours"))/24,0)*24))
  
  
  if(is.null(dfMeasureInterval) & nrow(dfMeasureInterval)==0){ return(NULL)}
  
  meanWhenDuration=When %>% interval2Dataframe() %>% mutate(dur=as.numeric(difftime(end,start,units="hours"))) %>% 
    summarise(dur=mean(dur,na.rm=T)) %>% pull()
  
  dateBreaks <- case_when(
    meanWhenDuration<= 1 ~ "5 min",
    meanWhenDuration<= 2 ~ "10 min",
    meanWhenDuration<= 6 ~ "30 min",
    TRUE ~                 "1 hour")
  
  
  
  
  formateaFechas=function(etiquetas){
    etiquetas2=etiquetas %>% ymd(etiquetas) %>% #formatear en texto en froma año-mes-dia
      format("%a %d/%b/%y") %>% str_replace_all("\\.","") 
    etiquetas3=etiquetas2 %>% str_replace("/[0-9][0-9]$","")
    etiquetas3[1]=etiquetas2[1]
    etiquetas3
  }
  
  
  dfGraph=dfCruce %>% inner_join(dfMeasureInterval, by = join_by(name)) %>% 
    mutate(name=as.factor(name)) %>% 
    mutate(startGraph= start-trasladar,
           endGraph=   end-trasladar) 
  
  levels(dfGraph$name) <- formateaFechas(dfGraph$name %>% levels())
  
  
  #  dfGraph %>% filter(what=="daily") %>% group_by(name) %>% summarise_all(first)
  
  dfLimits=dfGraph %>% filter(what==WhenName) %>% group_by(name) %>% summarise_all(first) %>%
    mutate(endGraph=startGraph+difftime(end,start)) %>% 
    select(name,startGraph,endGraph) %>% 
    pivot_longer(cols = c(startGraph,endGraph),names_to = "limit",values_to = "value")
  
  
  nombresIntervalosParaMostrar=nombresIntervalosParaMostrar %>% intersect(unique(dfGraph$what))
  dfGraph$what=fct_relevel(dfGraph$what,nombresIntervalosParaMostrar)
  
  message(dfGraph %>% count(name))
  
  grafico <- dfGraph %>%
    ggplot(aes(x = startGraph, y = what,color=what,tooltip=tooltip,data_id=data_id)) +
    geom_segment_interactive(aes(xend = endGraph, yend = what), linewidth = 1) +
    scale_color_manual(values = vectorColores) +
    scale_x_datetime(labels = function(date) {format(date, "%H:%M", tz = "Europe/Madrid")},
                     breaks = scales::breaks_width(dateBreaks)) +
    geom_vline(data=dfLimits,aes(xintercept = value),linetype="dotted",alpha=0.25)+
    facet_grid(name ~ .) +
    labs(y="", x="")+
    theme_bw() +
    theme(
      #      aspect.ratio = 0.025,
      legend.position = "bottom",
      legend.text = element_text(size = 6),
      legend.spacing.y = unit(0.1, "cm"),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size=7,angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      strip.background = element_blank(),
      strip.text.y.right = element_text(angle = 0),
      strip.text.y = element_text(size = 7),
      panel.spacing.y = unit(0, "lines")) +
    guides(color = guide_legend(
      title.theme = element_text(size = 8, face = "bold"), 
      label.theme = element_text(size = 6), 
      label.position = "right",
      label.hjust = 0,
      title.hjust = 0.5,
      label.spacing = unit(0.1, "cm")  # Reduce el espaciado entre etiquetas
    ))
  
  #  grafico = grafico+geom_vline(data=dfLimits,aes(xintercept = value),linewidth=1,alpha=0.25)
  
  grafico
  
}


##########################################################################################
# computeInvalids es la función que calcula las condiciones de validez según se indica en 
# el fichero de configuración.
# requiere de dos funciones auxiliares evaluar_validez y evalValidity que deberían ser privadas
#' Title
#'
#' @param Agregados1 
#' @param invalidationCriteria 
#'
#' @return
#' @export
#'
#' @examples
computeInvalids<- function(Agregados1,invalidationCriteria){
  invalidList=list()
  for(Reason in names(invalidationCriteria)){
    Concept=invalidationCriteria[[Reason]]
    dfInvalid=Agregados1 %>% map_df( evalValidity %>% partial(Expression=Concept$Expression,Reason=Reason))
    for (when in Concept$invalidates){
      invalidList[[when]] <- as_tibble(invalidList[[when]]) %>% bind_rows(dfInvalid)
    }
  }
  invalidList
}



evaluar_validez <- function(expresion, ...) {
  parametros <- list(...)
  env <- list2env(parametros)
  
  resultado <- tryCatch({
    as.logical(eval(parse(text = expresion), envir = env))
  }, error = function(e) {
    FALSE
  })
  
  return(resultado)
}  

evalValidity<-function(Agr1,Expression,Reason){
  Agr1 %>%
    mutate(.invalid=
             pmap_lgl(.,evaluar_validez %>% partial(expresion=Expression))) %>%
    filter(.invalid) %>% 
    mutate(.reason=Reason) %>% 
    select(name,.reason) 
}










#Nuevos drivers para leer datos de acelerometría de paquetes



#' Title
#'
#' @param path_biobank_ts 
#' @param baseBB 
#' @param RAW 
#' @param start 
#' @param end 
#' @param tz 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
readAccelerometryFromBioBank <- function(path_biobank_ts="",baseBB="",RAW="",start=NA,end=NA,tz = "Europe/Madrid",...){
  function(){
    if(path_biobank_ts == "" ) path_biobank_ts=sprintf("%s/%s",baseBB,str_replace(RAW,"\\.bin|\\.csv","-timeSeries.csv.gz"))
    
    dfBB=NULL
    
    try({
      dfBB <- read_csv(path_biobank_ts,show_col_types = FALSE) %>% 
        rename(timestamp=time) %>% 
        mutate(timestamp=ymd_hms(timestamp) %>% with_tz(tz)) %>% 
        mutate(acc=acc/1000) %>% 
        set_names(str_replace(names(.),"-","_"))
    })
    dfBB
  }
}


#' Title
#'
#' @param path_stepcount_ts 
#' @param baseStepCount 
#' @param RAW 
#' @param start 
#' @param end 
#' @param tz 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
readAccelerometryFromStepCount <- function(path_stepcount_ts="",baseStepCount="",RAW="",start=NA,end=NA,tz = "Europe/Madrid",...){
  function(){
    if(path_stepcount_ts == "" ) stop("No hay path_stepcount_ts")       
    #path_biobank_ts=sprintf("%s/%s",baseStepCount,str_replace(RAW,"\\.bin|\\.csv","-timeSeries.csv.gz"))
    
    dfStepCount=NULL
    
    try({
      dfStepCount <- read_csv(path_stepcount_ts,show_col_types = FALSE) %>% 
        filter(complete.cases(.)) %>% 
        rename(timestamp=time) %>% 
        mutate(timestamp=force_tz(timestamp,tz)) %>% 
        mutate(steps_acc=cumsum(Steps)) %>% 
        mutate(cadence=Steps/as.integer(difftime(timestamp,lag(timestamp)))) %>% 
        select(timestamp,cadence,steps_acc) %>% 
        filter(complete.cases(.))
      
    })
    dfStepCount
  }
}


#' Title
#'
#' @param path_ggir_ts 
#' @param base 
#' @param RAW 
#' @param start 
#' @param end 
#' @param tz 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
readAccelerometryForPorto <-function(path_ggir_ts="",base="",RAW="",start=NA,end=NA,tz = "Europe/Madrid",...){
  function(){
    if(path_ggir_ts == "" | is.null(path_ggir_ts) | is.na(path_ggir_ts) ) path_ggir_ts=sprintf("%s/meta/basic/meta_%s.RData",base,RAW)
    
    df=NULL
    
    try({
      
      if(file.exists(path_ggir_ts)) {
        
        load(path_ggir_ts)
        
        if(!is.null(M$metashort) & !is.null(M$metalong)){
          df=M$metashort %>%
            mutate(
              timestamp = with_tz(ymd_hms(timestamp),tz = "Europe/Madrid")
            ) %>%  select (timestamp,ENMO,anglez,everything()) %>% as_tibble()
          
          
          ##NonWear
          dfNW=M$metalong  %>%
            mutate(
              timestamp = with_tz(ymd_hms(timestamp),tz = tz),
              .criterioRaw=as.integer(nonwearscore!=0),#Ponemos el primer instante como nonWear
              .criterioBout=.criterioRaw) %>%
            select(timestamp,.criterioRaw,.criterioBout) %>% as_tibble()
          
          intervalosNW=dfNW %>% criterio2Interval() %>% #Eliminando nonWear cortos de noche
            filter(! (  (difftime(to,from)<dminutes(120) & ( hour(from)>22 | hour(to)<=8)) | difftime(to,from)<dminutes(40)))
          
          df=df %>% mutate(.criterioNW= interval2criterio(df$timestamp,intervalosNW))
        }
      }
    })
    #df
    
    #Cambios para PORTO
    df2= df %>% transmute(TS=timestamp, VM=as.integer(NeishabouriCount_vm)) %>%
      as.data.frame() %>% 
      markbedrest(cts="VM", age = "ps", loc="wa", rstdr = tempdir()  )
    
    df$bedrest=as.integer(df2$bedrest=="br")
    df
  }
}





#################################################################################################
# Función para crear un nuevo driver como resultado de la interpolación de una lista de ellos
# La referencia de las marcas horarias lo da el primer driver de la lista así que conviene
# que el primero sea el de resolución horaria ,más fina.
# 
#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
joinDrivers<- function(...){
  listaDrivers=list(...) %>% keep(is.function)
  if(!is.null(names(listaDrivers))) {
    listaDrivers = listaDrivers[str_detect(names(listaDrivers),"^driver_")]
  }
  function(...){
    dfResult=listaDrivers[[1]]()
    if(length(listaDrivers)==1) return(dfResult)
    
    for(i in 2:length(listaDrivers)){
      df2=listaDrivers[[i]]()
      if("timestamp" %in% names(df2)){
        varsToInterpolate=names(df2) %>% setdiff("tmestamp")
        for (v in varsToInterpolate){
          vFinal <- v
          contador <- 1
          while (vFinal %in% names(dfResult)) {
            vFinal <- paste(v, contador, sep = "_")
            contador <- contador + 1
          }
          
          dfResult[[vFinal]]=approx(x=df2$timestamp, y=df2[[v]], xout=dfResult$timestamp)$y
        }
      }
    }
    dfResult
  }
}







#' Title
#'
#' @param listaColorVariable 
#' @param intervalNames 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
palette2Color=function(listaColorVariable,intervalNames,...){
  if(names(listaColorVariable) %>% intersect(c("color","intervals")) %>% length()==2){
    return(
      colorRampPalette(listaColorVariable$color)(length(listaColorVariable$intervals))  %>%
        set_names(listaColorVariable$intervals) %>% .[names(.) %in% intervalNames]
    )
  }
  map(listaColorVariable,ColorVariable)
}



unlistColors<-function(lista){
  if(class(lista)=="character") return(lista)
  map(lista,unlistNames) %>% reduce(c)
}

unlistNames=function(lista){
  if(class(lista)=="character") return(lista)
  map(lista,unlistNames) %>% reduce(c)
}


#' Title
#'
#' @param intervalColor 
#' @param nombre 
#' @param intervalSelected 
#'
#' @return
#' @export
#'
#' @examples
intervalColor2checkbox=function(intervalColor,nombre,intervalSelected){
  if(class(intervalColor)=="character") {
    return(
      bslib::accordion_panel(
        id=nombre,
        title=nombre,
        shiny::checkboxGroupInput(inputId = nombre, 
                                  label = NULL, 
                                  choices = names(intervalColor),
                                  selected = intersect(names(intervalColor),intervalSelected)
        ),
        open=FALSE
      )
    )
  }
  map2(intervalColor,names(intervalColor),intervalColor2checkbox %>% partial(intervalSelected=intervalSelected)) 
}






###Codigo javascript para poder usar pickerInput respetando el orden del usuario.
# EL nombre del control de entrada se pasa como argumento.
# En la aplicacion hay que usar:
#tags$head(tags$script(jsPickerInput("nombreDelPickerInput")))

#' Title
#'
#' @param inputCtrl 
#'
#' @return
#' @export
#'
#' @examples
jsPickerInput <- function(inputCtrl){ HTML(
  str_c( "
$(function(){
    $('#",inputCtrl,"').on('change.bs.select loaded.bs.select',
        function(event) {
            $(this).find('option:selected').prependTo(this);
        });
});
")
)}



#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
dataframe2Interval <- function(df){
  result <- interval(df$start,df$end)
  if("name" %in% names(df)){
    if(length(df$name)==length(unique(df$name))){
      names(result) <- df$name}
  }
  
  result
}




#' Title
#'
#' @param intervalFrom 
#' @param intervalTo 
#' @param extremeOfFrom 
#' @param extremeOfTo 
#'
#' @return
#' @export
#'
#' @examples
interval_FromTo<-function(intervalFrom,intervalTo,extremeOfFrom="end",extremeOfTo="start"){
  excludeExtremeFrom=c("start","end") %>% setdiff(extremeOfFrom)
  excludeExtremeTo=c("start","end") %>% setdiff(extremeOfTo)
  
  whenFrom =intervalFrom %>% interval2Dataframe() %>% select(-all_of(excludeExtremeFrom))
  whenTo=intervalTo %>% interval2Dataframe()%>% select(-all_of(excludeExtremeTo))
  
  result=data.frame()
  
  if(!is.null(names(intervalFrom)) & !is.null(names(intervalTo))){
    result=whenFrom %>% transmute(name,start:= .data[[extremeOfFrom]]) %>%
      inner_join(
        whenTo %>% transmute(name,end := .data[[extremeOfTo]]) , join_by("name")) %>% 
      select(name,start,end) %>%  filter(start<end)
  } else {
    if(!is.null(names(intervalFrom))){
      result=whenFrom %>% transmute(name,start:= .data[[extremeOfFrom]]) %>% 
        inner_join(
          whenTo %>% transmute(end := .data[[extremeOfTo]]) ,
          join_by(closest(start<end))) %>% 
        select(name,start,end) 
    } else{
      if(!is.null(names(intervalTo))){
        
        result=whenFrom %>% transmute(start:= .data[[extremeOfFrom]]) %>% 
          inner_join(
            whenTo %>% transmute(name,end := .data[[extremeOfTo]]) ,
            join_by(closest(start<end))) %>% 
          select(name,start,end)
        
      } else {
        result=whenFrom %>% transmute(start:= .data[[extremeOfFrom]]) %>% 
          inner_join(
            whenTo %>% transmute(end := .data[[extremeOfTo]]) ,
            join_by(closest(start<end))) %>% 
          select(start,end)
      }
    }
  }
  
  result %>% dataframe2Interval()
}




#' Title
#'
#' @param fecha_inicio 
#' @param fecha_fin 
#'
#' @return
#' @export
#'
#' @examples
verificar_cambio_horario <- function(fecha_inicio, fecha_fin) {
  offset_inicio <- with_tz(fecha_inicio, tzone = "UTC")
  offset_fin <- with_tz(fecha_fin, tzone = "UTC")
  
  if (offset_inicio != offset_fin) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}





##############################################################################################################
# cfg transforma lo leído del fichero de configuración en una lista interpretada de objetos que son más fáciles
# de utilizar para el programador.
# Seguramente debería ser pública ya que será útil en las aplicaciones futuras que se realicen interpretando
# el fichero de configuración.
#' Title
#'
#' @param defineIntervals 
#' @param variablesLong 
#' @param invalidationCriteria 
#' @param intervalPalette 
#'
#' @return
#' @export
#'
#' @examples
cfg <- function(defineIntervals, variablesLong, invalidationCriteria, intervalPalette ){
  
  conf <- list(defineIntervals=defineIntervals, intervalPalette=intervalPalette, variablesLong=variablesLong, invalidationCriteria=invalidationCriteria)
  
  conf$intervalNames = names(defineIntervals) %>% keep(!str_detect(., "\\."))
  
  conf$menuInterval = map(intervalPalette,palette2Color %>% partial(intervalNames=conf$intervalNames)) %>% keep(negate(is.null)) %>% keep(~length(.)>0)
  
  conf$intervalColor = unlistColors(conf$menuInterval) %>% .[names(.) %in% conf$intervalNames]
  
  conf$intervalAggregated=variablesLong %>% map(~ {(.[["intervals"]]  %>% reduce(c))}) %>% unlist() %>% unique()
  conf$intervalPreSelected = conf$intervalNames %>% intersect(names(conf$intervalColor)) %>% intersect(conf$intervalAggregated)
  
  conf$intervalUI = intervalColor2checkbox(conf$menuInterval,names(conf$menuInterval),conf$intervalPreSelected) %>% set_names(NULL)
  
  conf$measureInterval = c("daily", "awake", "bed") %>%
    intersect(conf$intervalNames) %>%
    c(variablesLong %>% map(~ { (.[["measureIntervals"]]  %>% reduce(c))}) %>% unlist()) %>% unique()
  
  conf$aggregation = variablesLong %>%  map_chr(~ .[["Comentario"]]) %>% set_names(names(.), .)
  
  conf$driverNames = defineIntervals$.drivers
  conf
}

