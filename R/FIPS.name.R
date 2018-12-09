FIPS.name<-function(data,patco,level="county",add=c("name","FIPS"),state="36",county=NULL,map=NULL,long.case,lat.case,censusFIPS="GEOID"){
  COUNTY=data.frame(COUNTY=c("Albany","Allegany","Bronx","Broome",
                             "Cattaraugus","Cayuga","Chautauqua","Chemung","Chenango","Clinton","Columbia","Cortland","Delaware",
                             "Dutchess","Erie","Essex","Franklin","Fulton","Genesee","Greene","Hamilton","Herkimer","Jefferson",
                             "Kings","Lewis","Livingston","Madison","Monroe","Montgomery","Nassau","New York","Niagara","Oneida",
                             "Onondaga","Ontario","Orange","Orleans","Oswego","Otsego","Putnam","Queens","Rensselaer","Richmond","Rockland",
                             "Saratoga","Schenectady","Schoharie","Schuyler","Seneca","St Lawrence","Steuben","Suffolk","Sullivan",
                             "Tioga","Tompkins","Ulster","Warren","Washington","Wayne","Westchester","Wyoming","Yates"),
                    FIPS=c(36001,36003,36005,36007,36009,36011,36013,36015,36017,36019,36021,36023,36025,36027,36029,
                           36031,36033,36035,36037,36039,36041,36043,36045,36047,36049,36051,36053,36055,36057,36059,36061,
                           36063,36065,36067,36069,36071,36073,36075,36077,36079,36081,36083,36085,36087,36091,36093,36095,
                           36097,36099,36089,36101,36103,36105,6107,36109,36111,36113,36115,36117,36119,36121,36123),
                    SPARCS=c(1,2,58,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,59,23,24,25,26,27,28,60,29,
                             30,31,32,33,34,35,36,37,61,38,62,39,41,42,43,44,45,40,46,47,48,49,50,51,52,53,54,55,56,57))
  if(level=="county"){
    if("FIPS"%in%add) data$FIPS=COUNTY$FIPS[match(data[,patco],COUNTY$SPARCS)]
    if("name"%in%add) data$county=COUNTY$COUNTY[match(data[,patco],COUNTY$SPARCS)]
  }
  if(level=="census"){
    name=names(data)
    data$long.case=data[,long.case]
    data$lat.case=data[,lat.case]
    data$long.case=as.numeric(as.character(data$long.case))
    data$lat.case=as.numeric(as.character(data$lat.case))
    a=ncol(data)
    data1=data[which(!is.na(data$long.case)&!is.na(data$lat.case)),]
    sp::coordinates(data1)=~long.case+lat.case
    if(length(map)==0) NYSmap=tracts(state=state,county=county) else NYSmap=map
    raster::crs(data1)=raster::crs(NYSmap)
    data1=point.in.poly(data1,NYSmap)
    data1=as.data.frame(data1)
    data1=cbind(data1[,1:(a-2)],data1[,censusFIPS])
    names(data1)[ncol(data1)]="CSFIPS"
    data$CSFIPS=NA
    data$CSFIPS[which(!is.na(data$long.case)&!is.na(data$lat.case))]=as.character(data1$CSFIPS)
    data=data[,c(name,"CSFIPS")]
  }
  return(data)
}

