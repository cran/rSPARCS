desc.comp <-function(data,variables,by=NULL,margin=2,avg.num="mean",test.num="metric"){
  data0=as.data.frame(data[,variables])
  names(data0)=names(data)[variables]
  if(length(by)==0) group=rep(1,nrow(data0)) else group=data[,by]
  if(is.factor(group)) level=levels(group) else level=unique(group)[order(unique(group))]
  Output=NULL
  for(i in 1:length(variables)){
    data_med=data.frame(a=data0[,i],group=group)
    names(data_med)[1]=names(data0)[i]
    if(is.numeric(data0[,i])|is.integer(data0[,i])){
      result=matrix(rep(NA,1*(length(level)+2)),nrow=1)
      mean.in=sprintf("%.2f",tapply(data_med[,1],data_med[,2],mean,na.rm=T))
      sd.in=sprintf("%.2f",tapply(data_med[,1],data_med[,2],sd,na.rm=T))
      median.in=sprintf("%.2f",tapply(data_med[,1],data_med[,2],median,na.rm=T))
      IQR.in=sprintf("%.2f",tapply(data_med[,1],data_med[,2],IQR,na.rm=T))
      test=NULL
      try({test=tapply(data_med[,1],data_med[,2],shapiro.test)},silent=T)
      shapiro=rep(0.5,length(level))
      if(length(test)==0) print("Warning: The sample size for some groups may be too small") else {
        for(k in 1:length(test)) shapiro[k]=test[[k]]$p.value
      }
      if(max(shapiro)>=0.05) result[1,1:length(level)]=paste(mean.in," (",sd.in,")",sep="")  else result[1,1:length(level)]=paste(median.in," (",IQR.in,")",sep="")
      if(avg.num=="mean") result[1,1:length(level)]=paste(mean.in," (",sd.in,")",sep="")
      if(avg.num=="median") result[1,1:length(level)]=paste(median.in," (",IQR.in,")",sep="")
      result[,(ncol(result)-1):ncol(result)]=""
      if((max(shapiro)>=0.05&test.num!="nonmetric")|test.num=="metric"){
        if(length(level)==2){
          result[1,ncol(result)-1]=paste("t =",sprintf("%.2f",t.test(data_med[,1]~data_med[,2],var.equal=T)$statistic))
          test=t.test(data_med[,1]~data_med[,2],var.equal=T)$p.value
          test=ifelse(test<0.001,"P<0.001",sprintf("%.3f",test))
          result[1,ncol(result)]=test
        }
        if(length(level)>2){
          result[1,ncol(result)-1]=paste("F =",sprintf("%.2f",summary(aov(data_med[,1]~data_med[,2]))[[1]]$"F value"[1]))
          test=summary(aov(data_med[,1]~data_med[,2]))[[1]]$"Pr(>F)"[1]
          test=ifelse(test<0.001,"P<0.001",sprintf("%.3f",test))
          result[1,ncol(result)]=test
        }
        
      } else{
        if(length(level)==2){
          result[1,ncol(result)-1]=paste("W =",sprintf("%.2f",wilcox.test(data_med[,1]~data_med[,2],exact=F)$statistic))
          test=wilcox.test(data_med[,1]~data_med[,2],exact=F)$p.value
          test=ifelse(test<0.001,"P<0.001",sprintf("%.3f",test))
          result[1,ncol(result)]=test
        }
        if(length(level)>2){
          result[1,ncol(result)-1]=paste("W =",sprintf("%.2f",kruskal.test(data_med[,1]~data_med[,2])$statistic))
          test=kruskal.test(data_med[,1]~data_med[,2])$p.value
          test=ifelse(test<0.001,"P<0.001",sprintf("%.3f",test))
          result[1,ncol(result)]=test
        }
      }
      rownames(result)=names(data0)[i]
      colnames(result)=c(level,"statistic","P")
    } else {
      result=matrix(rep(NA,(length(levels(data_med[,1]))+1)*(length(level)+2)),nrow=length(levels(data_med[,1]))+1)
      media=table(data_med[,1],data_med$group)
      media1=round(prop.table(media,margin)*100,2)
      media=as.data.frame.matrix(media)
      media1=as.data.frame.matrix(media1)
      for(k in 1:ncol(media1)){
        media1[,k]=sprintf("%.2f",media1[,k])
        result[-1,k]=paste(media[,k]," (",media1[,k],")",sep="")
      }
      result[1,]=""
      result[,(ncol(result)-1):ncol(result)]=""
      set.seed(4715)
      if(length(level)>1){
        result[2,ncol(result)-1]=paste("Chisq =",sprintf("%.2f",chisq.test(as.matrix(media),simulate.p.value=T)$statistic))
        set.seed(4715)
        test=chisq.test(as.matrix(media),simulate.p.value=T)$p.value
        test=ifelse(test<0.001,"P<0.001",sprintf("%.3f",test))
        result[2,ncol(result)]=test
      }
      rownames(result)=c(names(data0)[i],rownames(media))
      colnames(result)=c(level,"statistic","P")
    }
    for(k in 1:ncol(result)) result[,k]=as.character(result[,k])
    Output=rbind(Output,result)
  }
  return(Output)
}
