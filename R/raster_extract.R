raster_extract=function(rastermap,refmap,ID.var="FIPS",ID.code="ALL",cutpoint=NULL){
  message("Wait! It may take some time to uniform the projection systems of the rastermap and refmap")
  refmap=spTransform(refmap,CRS(projection(rastermap)))
  areasnum=ifelse(ID.code=="ALL",length(refmap),length(ID.code))
  refmap1=as.data.frame(refmap)
  if(ID.code=="ALL") areasname=refmap1[,ID.var] else areasname=ID.code
  result=NULL
  for(i in 1:length(areasname)){
    crop_ref=refmap[which(refmap1[,ID.var]==areasname[i]),]
    crop_raster=crop(rastermap, extent(crop_ref))
    crop_raster=mask(crop_raster,crop_ref)
    ex.test=extract(crop_raster,crop_ref,cellnumbers=TRUE,df=TRUE)
    names(ex.test)[names(ex.test)==names(rastermap)]="value"
    if(!is.null(cutpoint)){
      ex.test$value=as.numeric(as.character(ex.test$value))
      ex.test=ex.test[!is.na(ex.test$value),]
      ex.test$test=NA
      ex.test$test[ex.test$value<cutpoint]=paste("<",cutpoint)
      ex.test$test[ex.test$value>=cutpoint]=paste(">=",cutpoint)
      ex.test$value=ex.test$test
    }
    output=as.data.frame(matrix(NA,1,length(unique(ex.test$value))+1))
    names(output)[1]="ID.code"
    output$ID.code=areasname[i]
    names(output)[-1]=unique(ex.test$value)
    rownames(output)=i
    for(j in 1:length(unique(ex.test$value))){
      test=ex.test[which(ex.test$value==as.character(colnames(output)[j+1])),]
      output[1,j+1]=nrow(test)
    }
    result=rbind.fill(result,output)
  }
  result$"Total cells"=NA
  for(i in 1:nrow(result)) result$"Total cells"[i]=sum(result[i,2:(ncol(result)-1)],na.rm=T)
  return(result)
}
