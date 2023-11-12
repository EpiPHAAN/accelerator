ggir2Interval<- function(base,RAW,start=NA,end=NA,nucleos=numCores(4),...) {
  ggir2ts(base,RAW,start,end) %>% ts2Interval(nucleos,...)
  }