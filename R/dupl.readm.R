dupl.readm=function(data,UniqueID,date,period=365){
  data$d2=data$d3=NA
  test=as.character(data[,date])
  test=test[test!=""&!is.na(test)]
  if(any(!is.na(grep("/",test[1])),!is.na(grep("-",test[1])))) data[,date]=as.Date(as.character(data[,date])) else  data[,date]=as.Date(paste(substr(data[,date],1,4),"/",substr(data[,date],5,6),"/",substr(data[,date],7,8),sep=""))
  
  test=which(duplicated(data[,c(UniqueID,date)]))
  data$id.dupl=0
  data$id.dupl[test]=1
  data0=data[data$id.dup==0,]
  data1=data[data$id.dup==1,]#save
  if(nrow(data1)>0) data1$onlyone=0
  test=which(data0[,UniqueID]%in%data0[which(duplicated(data0[,UniqueID])),UniqueID])
  data0$onlyone=1
  data0$onlyone[test]=0
  
  test=unique(data0[data0$onlyone==0,UniqueID])
  
  # admission time
  for(i in 1:length(test)){
    med=data0[data0[,UniqueID]==test[i],]
    med=med[order(med[,date]),]
    med$d2=c(0,diff(med[,date]))
    med$d3=ifelse(med$d2<=period,1,0)
    med$d3[1]=0
    med1=data.frame(x=rep(rle(med$d3)$values,rle(med$d3)$lengths),
                    y=sequence(rle(med$d3)$lengths))
    med1$y[med1$x==0]=0
    med$d3=med1$y
    data0[data0[,UniqueID]==test[i],]=med
  }
  
  data=rbind(data0,data1)
  names(data)[names(data)=="d3"]="Nadmission"
  names(data)[names(data)=="d2"]="Period"
  return(data)
}



