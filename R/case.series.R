case.series <-function(data,ICD=NULL,diagnosis,date="ADMDT",start="2001/1/1",end="2016/12/31",by1=NULL,by2=NULL,by3=NULL,by4=NULL,by5=NULL){
  data$diag6=data$diag5=data$diag4=data$diag3=NA
  data$diag3=substr(data[,diagnosis],1,3)
  data$diag4=substr(data[,diagnosis],1,4)
  data$diag5=substr(data[,diagnosis],1,5)
  data$diag6=substr(data[,diagnosis],1,6)
  data=data[data$diag3%in%ICD|data$diag4%in%ICD|data$diag5%in%ICD|data$diag6%in%ICD,]
  test=as.character(data[,date])
  test=test[test!=""&!is.na(test)]
  if(any(!is.na(grep("/",test[1])),!is.na(grep("-",test[1])))) data[,date]=as.Date(as.character(data[,date])) else  data[,date]=as.Date(paste(substr(data[,date],1,4),"/",substr(data[,date],5,6),"/",substr(data[,date],7,8),sep=""))
  by=c(by1,by2,by3,by4,by5)
  if(length(by)>0) by=by[!is.na(by)]

  if(length(by)>0){
    for(k in 1:length(by)){
      a=paste(unique(data[,by[k]]))
      b=paste("Level for", by[k],":",a[1])
      for(t in 2:length(a)) b=paste(b,a[t])
      message(b)
      data$var.by=data[,by[k]]
      names(data)[ncol(data)]=paste("var",k,sep="")
    }
    case=var1=var2=var3=var4=var5=date.test=NA
    data$case=1
    med=as.data.table(data)
    if(length(by)==1) med=as.data.frame(med[,list(case=sum(case)),list(var1)])
    if(length(by)==2) med=as.data.frame(med[,list(case=sum(case)),list(var1,var2)])
    if(length(by)==3) med=as.data.frame(med[,list(case=sum(case)),list(var1,var2,var3)])
    if(length(by)==4) med=as.data.frame(med[,list(case=sum(case)),list(var1,var2,var3,var4)])
    if(length(by)==5) med=as.data.frame(med[,list(case=sum(case)),list(var1,var2,var3,var4,var5)])
    names(med)[1:(ncol(med)-1)]=by
    Output1=med

    data$date.test=data[,date]
    data=as.data.table(data)
    if(length(by)==1) data=as.data.frame(data[,list(case=sum(case)),list(date.test,var1)])
    if(length(by)==2) data=as.data.frame(data[,list(case=sum(case)),list(date.test,var1,var2)])
    if(length(by)==3) data=as.data.frame(data[,list(case=sum(case)),list(date.test,var1,var2,var3)])
    if(length(by)==4) data=as.data.frame(data[,list(case=sum(case)),list(date.test,var1,var2,var3,var4)])
    if(length(by)==5) data=as.data.frame(data[,list(case=sum(case)),list(date.test,var1,var2,var3,var4,var5)])
    data$code=data[,"date.test"]
    for(t in 1:(ncol(Output1)-1)) data$code=paste(data$code,data[,paste("var",t,sep="")])

    Output2=NULL
    for(k in 1:nrow(Output1)){
      med=data.frame(date=seq.Date(as.Date(start),as.Date(end),"1 day"))
      for(t in 1:(ncol(Output1)-1)){
        med$var=Output1[k,t]
        names(med)[ncol(med)]=names(Output1)[t]
      }
      med$code=med$date
      for(t in 1:(ncol(Output1)-1)) med$code=paste(med$code,med[,t+1])
      med$case=data$case[match(med$code,data$code)]
      med$case[is.na(med$case)]=0
      med=med[,-which(names(med)=="code")]
      Output2=rbind(Output2,med)
    }
  } else{
    data$date.test=data[,date]
    data$case=1
    data=as.data.table(data)
    data=as.data.frame(data[,list(case=sum(case)),list(date.test)])
    med=data.frame(date=seq.Date(as.Date(start),as.Date(end),"1 day"))
    med$case=data$case[match(med$date,data$date.test)]
    med$case[is.na(med$case)]=0
    Output1=NULL
    Output2=med
  }
  message(Output1)
  return(Output2)
}





