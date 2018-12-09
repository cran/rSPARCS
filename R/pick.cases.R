pick.cases<-function(data,long.case,lat.case,long.sites,lat.sites,radius="15 miles"){
  data$which.site=NA
  a=ncol(data)
  for(i in 1:length(long.sites)){
    data$var=NA
    data$var=distGeo(data[,c(long.case,lat.case)],c(long.sites[i],lat.sites[i]))*0.000621371
    if(gregexpr("km",radius)[[1]][1]>0|gregexpr("kms",radius)[[1]][1]>0) data$var=data$var*1.60934
    if(i==1) data$which.site[!is.na(data$var)]=1
    if(i==1) names(data)[ncol(data)]=paste("distance.site",i,sep="")
    if(i>1) data$which.site=ifelse(data$distance.site1<data$var,data$which.site,i)
    if(i>1) data$distance.site1=ifelse(data$distance.site1<data$var,data$distance.site1,data$var)
  }
  data=cbind(data[,1:a],data[,"distance.site1"])
  names(data)[ncol(data)]="minDIST"
  a=gregexpr("km",radius)[[1]][1]
  b=gregexpr("kms",radius)[[1]][1]
  c=gregexpr("mile",radius)[[1]][1]
  d=gregexpr("miles",radius)[[1]][1]
  cut=as.numeric(substr(radius,1,unique(c(a,b,c,d)[c(a,b,c,d)>0])-2))
  data$Select=0
  data$Select[data$minDIST<=cut]=1
  return(data)
}

