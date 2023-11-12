changedDST<- function(inicio,final){
data.frame(timestamp=seq(ceiling_date(inicio,unit="hours"),floor_date(final,unit="hours"),by = 60)) %>% 
    mutate(.criterioBout=(dst(timestamp)!=dst(inicio))) %>% 
    criterio2Interval() %>% .[["from"]]
}
