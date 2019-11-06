mediationking<-function(dataset,outcome,mediator,exposure,n.sim=1000){
  y=outcome
  m=mediator
  x=exposure
  table=NULL
  tb<-NULL
  for (i in 1:n.sim){
    
    data=dataset[sample(1:nrow(dataset),size=round(nrow(dataset)*0.75),replace=TRUE),]
    fit.yx<-glm(as.formula(paste(y,"~",x,sep="")),data=data)
    sum.yx<-summary(fit.yx)
    c<-sum.yx$coefficients[2,1]
    i1<-sum.yx$coefficients[1,1]
                
    if (length(table(data[m,]))==2){
      fit.mx<-glm(as.formula(paste(m,"~",x,sep="")),data = data,family=binomial(link="logit"))
      sum.mx<-summary(fit.mx)
      a<-sum.mx$coefficients[2,1]
      i2<-sum.mx$coefficients[1,1]
    }else{                        
      fit.mx<-glm(as.formula(paste(m,"~",x,sep="")),data=data)
      sum.mx<-summary(fit.mx)
      a<-sum.mx$coefficients[2,1]
      i2<-sum.mx$coefficients[1,1]
    }
                                       
    if (length(table(data[y,]))==2){
          fit.yxm<-lm(as.formula(paste(y,"~",x,"+",m,sep="")),data=data,family=binomial(link="logit"))
          sum.yxm<-summary(fit.yxm)
          b<-sum.yxm$coefficients[3,1]
          cpr<-sum.yxm$coefficients[2,1]
           i3<-sum.yxm$coefficients[1,1]
    }else{                         
      fit.yxm<-lm(as.formula(paste(y,"~",x,"+",m,sep="")),data=data)
      sum.yxm<-summary(fit.yxm)
      b<-sum.yxm$coefficients[3,1]
      cpr<-sum.yxm$coefficients[2,1]
      i3<-sum.yxm$coefficients[1,1]
    }
    table<-matrix(c(c,a*b,cpr,a*b/c,a,b,i1,i2,i3),1,9)
    if(is.null(tb)){
      tb<-table
    }else{
      tb<-rbind(tb,table)
    }
  }

  total<-round(median(tb[,1]),2)
  total_u<-round(quantile(tb[,1],0.975),2)
  total_l<-round(quantile(tb[,1],0.025),2)
  total_final<-matrix(c(total,total_l,total_u),1,3)
  
  indirect<-round(median(tb[,2]),2)
  indirect_u<-round(quantile(tb[,2],0.975),2)
  indirect_l<-round(quantile(tb[,2],0.025),2)
  indirect_final<-matrix(c(indirect,indirect_l,indirect_u),1,3)
  
  direct<-round(median(tb[,3]),2)
  direct_u<-round(quantile(tb[,3],0.975),2)
  direct_l<-round(quantile(tb[,3],0.025),2)
  direct_final<-matrix(c(direct,direct_l,direct_u),1,3)
  
  prop<-round(median(tb[,4]),2)
  prop_u<-round(quantile(tb[,4],0.975),2)
  prop_l<-round(quantile(tb[,4],0.025),2)
  prop_final<-matrix(c(prop,prop_l,prop_u),1,3)
  
  final<-rbind(total_final,indirect_final)
  final<-rbind(final,direct_final)
  final<-rbind(final,prop_final)
  colnames(final)<-c("Estimate","95% LB","95% UB")
  rownames(final)<-c("Total effect","Indirect effect","Direct effect","Meditation.proportion")
  return(final)
}