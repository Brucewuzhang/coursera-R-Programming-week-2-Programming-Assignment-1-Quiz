##pollutantmean.R
pollutantmean <- function(directory,pollutant,id=1:332){
  s=0 ##s will be sum of not NA days 
  p=0 ##p will be the sum of pollution
  for (i in id) {
    if (i<10){
      i=as.character(i)
      j=paste("00",i,".csv",sep="")
    } else if (i<100) {
      i=as.character(i)
      j=paste("0",i,".csv",sep="")
    } else{
      i=as.character(i)
      j=paste(i,".csv",sep="")
    }
    fname = paste(directory,j,sep="/")
    f=read.csv(fname) ## read file i.csv
    a=is.na(f[pollutant])
    s=s+sum(!a)
    p=p+sum(f[pollutant][!a])
  }
  p/s ##total mean
} 