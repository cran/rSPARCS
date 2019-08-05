DBFgeocode <- function(data,cityname,roadaddress,mailbox=NULL,ZIP){  
  if(length(mailbox)==0) {
    addr12=data[,roadaddress]
  } else{
    test1=nchar(as.character(data[,mailbox]))
    test2=paste(data[,roadaddress],data[,mailbox])
    addr12=ifelse(test1==0,data[,roadaddress],test2)
  }
  cityzip=paste(data[,cityname],data[,ZIP])
  data$singleline=paste(addr12,", ",cityzip,sep="")
  return(data)
  print("You may output the data as the dbf file using write.dbf ()")
}

