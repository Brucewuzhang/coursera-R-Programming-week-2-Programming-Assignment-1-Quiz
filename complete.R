complete <- function(directory,id=1:332){
  df = data.frame(integer(),integer()) ## set an empty dataframe
  for (i in id) {
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
    n=sum((!a)*(!b))  ##number of complete observations
    df=rbind(df,c(i,n))
  }
  colnames(df) = c("id","nobs")
  df
}