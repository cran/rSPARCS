CXover.data=function(data,date,ID=NULL,direction="pre4"){
  name=names(data)
  if(length(ID)==0) data$ID=1:nrow(data) else data$ID=factor(data[,ID],levels=unique(data[,ID]))
  test=as.character(data[,date])
  test=test[test!=""&!is.na(test)]
  if(any(!is.na(grep("/",test[1])),!is.na(grep("-",test[1])))) data$d1=as.Date(as.character(data[,date])) else  data$d1=as.Date(paste(substr(data[,date],1,4),"/",substr(data[,date],5,6),"/",substr(data[,date],7,8),sep=""))
  data$date1=as.character(data$d1-28)
  data$date2=as.character(data$d1-21)
  data$date3=as.character(data$d1-14)
  data$date4=as.character(data$d1-7)
  data$date5=as.character(data$d1+7)
  data$date6=as.character(data$d1+14)
  data$date7=as.character(data$d1+21)
  data$date8=as.character(data$d1+28)
  data$date1_1=substr(data$date1,6,7)
  data$date2_1=substr(data$date2,6,7)
  data$date3_1=substr(data$date3,6,7)
  data$date4_1=substr(data$date4,6,7)
  data$date5_1=substr(data$date5,6,7)
  data$date6_1=substr(data$date6,6,7)
  data$date7_1=substr(data$date7,6,7)
  data$date8_1=substr(data$date8,6,7)
  if(direction=="pre4") data$date5=data$date6=data$date7=data$date8=NA
  if(direction=="month4"){
    data$d1_1=substr(data$d1,6,7)
    data$date1=ifelse(data$date1_1==data$d1_1,data$date1,NA)
    data$date2=ifelse(data$date2_1==data$d1_1,data$date2,NA)
    data$date3=ifelse(data$date3_1==data$d1_1,data$date3,NA)
    data$date4=ifelse(data$date4_1==data$d1_1,data$date4,NA)
    data$date5=ifelse(data$date5_1==data$d1_1,data$date5,NA)
    data$date6=ifelse(data$date6_1==data$d1_1,data$date6,NA)
    data$date7=ifelse(data$date7_1==data$d1_1,data$date7,NA)
    data$date8=ifelse(data$date8_1==data$d1_1,data$date8,NA)
  }
  if(direction!="pre4"&direction!="month4") print("You may contact author (wzhang27@albany.edu) to add more options")
  output=data[,c("ID","d1",name)]
  names(output)[2]="Date"
  output$status=1
  for(j in 1:8){
    test=data[,c("ID",paste("date",j,sep=""),name)]
    names(test)[2]="Date"
    test=test[which(!is.na(test$Date)),]
    if(nrow(test)>0){
      test$status=0
      output=rbind(output,test)
    }
    output=output[order(output$ID),]
  }
  rownames(output)=1:nrow(output)
  return(output)
}
