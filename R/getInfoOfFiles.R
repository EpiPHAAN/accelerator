#' Title
#'
#' @param dir 
#' @param prefix 
#' @param suffix 
#'
#' @return
#' @export
#'
#' @examples
getInfoOfFiles=function(dir,prefix="",suffix="\\.RData$"){
  if(is.null(prefix) | is.na(prefix) | !is.character(prefix)) prefix=""
  prefixSearch=prefix
  if(prefix!="") prefixSearch=str_c("^",prefix)
  
  
  
  dfResult=dir %>% map_df(~ data.frame(file=list.files(path = dir,pattern = str_c(prefixSearch,".*",suffix)))) %>%
    mutate(file=as.character(file))%>% 
    as_tibble() %>%
    mutate(path=str_c(dir,"/",file),
           RAW= str_replace(file,"\\.RData$","")) %>%
    mutate(info=map(path,file.info)) %>%
    mutate(size=map_dbl(info,~ .[["size"]]),
           mtime=map(info,~ .[["mtime"]]) %>% unlist() %>% as.POSIXct(origin="1970-01-01")) %>%
    select(-info)
  
  if(prefix!="") dfResult= dfResult %>% mutate(RAW=str_replace(RAW,prefix,"")) 
  dfResult
}
