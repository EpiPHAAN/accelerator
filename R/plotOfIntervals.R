#' Title
#'
#' @param df 
#' @param whatColumn 
#' @param whenColumn 
#' @param whenConcept 
#' @param what2Color 
#' @param epochColumnColor 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
plotOfIntervals=function(df,whatColumn="what",whenColumn="when",whenConcept="daily",what2Color,epochColumnColor,size=1,...){
  dfInt=df %>% select(RAW,what,when) %>% 
    ###      mutate(what=map(what,~list2df(.x,label="what")),
    mutate (what= map2(what,when, ~ {.x  %>% list2df(label="what") %>% select(what,from,to) %>% intervalIntersectv2(.y %>% pluck(whenConcept)) %>% select(-from,-to) %>% rename(from=fromNew,to=toNew)} )) %>%
    select(-when) %>%
    unnest(cols=c(what)) %>%
    mutate(NUM=as.integer(as.factor(RAW))) %>%
    mutate(order= map2_dbl(.[["what"]],.[["NUM"]], ~ (what2Color %>% pluck(.x,"order")+10*.y))) %>%
    mutate(color= map_chr(.[["what"]],   ~ what2Color %>% pluck(.x,"color")) ) %>%
    mutate(zindex= map_dbl(.[["what"]],   ~ {zindex=what2Color %>% pluck(.x,"zindex") %>% as.numeric(); ifelse(length(zindex)==1,zindex,1)}) )
  
  when=df %>% pluck("when") %>% map_df(~ .[[whenConcept]]) %>% group_by(day,label) %>%
    summarise(from=min(from),to=max(to)) %>% ungroup()
  
  
  
  zona=tz(dfInt$from[1])
  inicio=ceiling_date(min(dfInt$from),unit="minutes")
  final=floor_date(max(dfInt$to),unit="minutes")
  cambioDST=changedDST(inicio,final)
  
  message(cambioDST)
  dfInt=dfInt %>% mutate(etiquetaDia=str_sub(as.character(as_date(day)),3,10))
  
  if (length(cambioDST)==1)  {
    dfInt$etiquetaDia[dfInt$day==as_date(cambioDST)]=str_c(str_sub(as.character(as_date(cambioDST)),3,10),"***")
  }

  
  primero=as_date(inicio)
  ultimo=as_date(final)
  dias=primero+(0:(1*0+ultimo-primero))
  
  dfIntDias=dfInt %>% #intervalIntersectv2(when) %>%
    # select(-from,-to) %>% rename(from=fromNew,to=toNew) #%>%
    mutate(
      from_b= from-(day-primero),
      to_b= to-(day-primero),
      dia= str_c(etiquetaDia,"\n",iconv(weekdays(day,abbreviate=TRUE),  to = 'ASCII//TRANSLIT')),
      data_id=str_c(what,";",from,";",to,";",RAW))   %>%
    arrange(zindex,what,from_b)
  
  #print(dfIntDias)
  
  miPaleta1=dfIntDias %>% distinct(what,color)
  miPaleta=miPaleta1$color %>% set_names(miPaleta1$what)
  
  grafico=ggplot(dfIntDias,aes(x=from_b,y=order))+
    ggiraph::geom_segment_interactive(aes(xend=to_b,yend=order,color=what,tooltip=data_id,data_id = data_id),size=size)+
    scale_x_datetime(labels=date_format("%H",tz=zona),date_minor_breaks="30 mins",date_breaks="1 hours",position = "top")+
    #,limits=c(desdeGrafico,hastaGrafico)
#    theme_stata() +
    scale_y_continuous(breaks=NULL)+
    xlab("Hora")+
    scale_color_manual(values=miPaleta)+
    theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.x=element_text(size=10),
      panel.grid.major.x=element_line(colour = 'lightblue'),
      axis.title.y=element_blank(),
      legend.position="top",
      legend.title = element_blank(),
      strip.text.y = element_text(size = 9,margin=margin()),
      panel.spacing.y=unit(0.05, "lines")
    )
  
  
  
  
  
  if (sum(!is.na(dfIntDias$dia))>0)  grafico=grafico+facet_wrap(~dia,ncol = 1,strip.position="left")
  
  grafico
}
