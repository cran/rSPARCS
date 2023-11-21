\name{raster_extract}
\alias{raster_extract}
\title{Extract Values from a Raster Map}
\description{Crop the raster with the boundary of areas of your interest, and extract the values from the raster to each of these areas.}
\usage{raster_extract(rastermap,refmap,ID.var,ID.code,cutpoint)}
\arguments{
  \item{rastermap}{a raster map containing the information you need, such as the National Land Cover Database 2011.}
  \item{refmap}{"SpatialPolygonsDataFrame" object. A reference map containing the boundary information of your study areas.}
  \item{ID.var}{the name of variable in the refmap indicating the unique ID for each of your study areas.}
  \item{ID.code}{a character vector containing the unique ID for areas that you want to extract the values to. ID.code=ALL" by default where all areas in the reference map are of interest.}
  \item{cutpoint}{a number to dichotomize the values in the raster; specified ONLY when those values are continuous.}
  }
\details{Usually for extracting data which are available as rasters such as the land coverage or land usage data.}
\value{
  \item{ID.code}{the column indicating the unique ID for each area, followed by the number of cells for each category/colour within that area.}
  \item{Total cells}{the total number of cells within each area.}
}
\examples{
library(raster)
set.seed(4715)
rast=raster(matrix(rnorm(500),100,100))
extent(rast)=c(50,100,10,60)
crs(rast)=CRS("+proj=longlat +datum=WGS84")

ref=cbind(x=c(60,80,80,70), y=c(20,25,40,30))
p=Polygon(ref)
ps=Polygons(list(p),ID="ID")
ref=SpatialPolygons(list(ps))
data=data.frame(value=1, ID="10086",row.names="ID")
ref=SpatialPolygonsDataFrame(ref,data)
proj4string(ref)=CRS("+proj=longlat +datum=WGS84")

raster_extract(rastermap=rast,refmap=ref,ID.var="ID",ID.code="ALL",cutpoint=0.5)
}
