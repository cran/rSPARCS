exposure_lag<-function(data,var,maxlag,ID,Date,lag_suffix=c('_cu_lag','_si_lag')){
  data=as.data.table(data)
  data=data.table::setorderv(data,c(ID,Date),c(1,1))
  
  for (i in var) {
    for (lag in 1:maxlag) {
      data[,paste0(i,lag_suffix[2],lag)]=data[,stats::filter(get(i),c(rep(0,lag),1), sides=1),by=ID][,2]# single lag
      
      data[,paste0(i,lag_suffix[1],lag)]=data[,stats::filter(get(i),rep(1/(lag+1),(lag+1)), sides=1),by=ID][,2]# cumulative lag
    }
  }
  return(data)
}

