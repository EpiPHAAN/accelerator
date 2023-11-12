ggir2IntervalV2<- function(currentProject,base,RAW,start=NA,end=NA,nucleos=numCores(4),...) {
  ggir2ts(base,RAW,start,end) %>% ts2IntervalV2(currentProject,nucleos,...)
  }