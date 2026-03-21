#' @title Retrieve Information of Files in a Directory
#'
#' @description Scans a directory for files matching a specific prefix and suffix, and extracts metadata such as full path, size, modify time (mtime), and the base variable name without the prefix/suffix.
#'
#' @param dir A string indicating the path to the directory to scan.
#' @param prefix A string for the required file prefix. If empty, it is ignored.
#' @param suffix A regex pattern for the required file suffix. Defaults to \\code{"\\\\.RData$"}.
#'
#' @return A tibble with columns: \code{file}, \code{path}, \code{RAW} (the clean name), \code{size}, and \code{mtime}.
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
