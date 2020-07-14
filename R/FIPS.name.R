FIPS.name<-function(data,ID.case,long.case,lat.case,map=NULL,state.map,level.map="wz",areaID="GEOID"){
  name=names(data)
  data$ID=data[,ID.case]
  data$long.case=data[,long.case]
  data$lat.case=data[,lat.case]
  data$long.case=as.numeric(as.character(data$long.case))
  data$lat.case=as.numeric(as.character(data$lat.case))
  a=ncol(data)
  data1=data[which(!is.na(data$long.case)&!is.na(data$lat.case)),]
  sp::coordinates(data1)=~long.case+lat.case
  if(is.null(map)&level.map=="county") NYSmap=tigris::counties(state=state.map) else NYSmap=map
  if(is.null(map)&level.map=="tract") NYSmap=tigris::tracts(state=state.map) else NYSmap=map
  raster::crs(data1)=raster::crs(NYSmap)
  data1=spatialEco::point.in.poly(data1,NYSmap)
  data1=as.data.frame(data1)
  data1=cbind(data1[,1:(a-2)],data1[,areaID])
  names(data1)[ncol(data1)]="areaID"
  data$areaID=NA
  data$areaID[which(!is.na(data$long.case)&!is.na(data$lat.case))]=as.character(data1$areaID)
  data=data[,c(name,"areaID")]
  return(data)
}
