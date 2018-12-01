corr <- function(directory, threshold = 0){
  v=numeric() ## set an empty vector
  for (i in 1:332) {
    if (i<10){
      j=paste("00",as.character(i),".csv",sep="")
    } else if (i<100) {
      j=paste("0",as.character(i),".csv",sep="")
    } else{
      j=paste(as.character(i),".csv",sep="")
    }
    fname = paste(directory,j,sep="/")
    f=read.csv(fname) ## read file i.csv
    a=is.na(f$su) ## NA sul value
    b=is.na(f$ni) ## NA ni value
    c=(!a)*(!b)
    n=sum(c)  ##number of complete observations
    if (n > threshold){
      v = c(v,cor(f$su,f$ni,use="complete.obs"))
    }
  }
  v
}