\name{FIPS.name}
\alias{FIPS.name}
\title{Determine the Area that Each Record Is Located in}
\description{Identify the residential county/city/census tract for each case, and add county/city/census tract ID.}
\usage{FIPS.name(data,ID.case,long.case,lat.case,map,state.map,level.map,areaID)}
\arguments{
  \item{data}{A data.frame containing the ID and coordinates of cases}
  \item{ID.case}{Name of the variable in the data indicating the case ID.}
  \item{long.case}{Name of the variable in the data indicating the longitude of cases.}
  \item{lat.case}{Name of the variable in the data indicating the latitude of cases.}
  \item{map}{The reference map containing the boundary of county/city/census tract. Do not have to specify for study areas within the U.S. A map for a region outside the U.S. can be imported as a "spatialpolygonsdataframe" object.}
  \item{state.map}{State FIPS code for the study area, e,g, "36" for the New York State. Ignored if readers' own map is being used.}
  \item{level.map}{"county" or "tract", determine whether cases will be macthed to counties or census tracts. Ignored if readers' own map is being used.}
  \item{areaID}{Name of the variable in the map indicating the area ID. Use the default if the study is within the U.S.}
}
\details{Not limited to hospital data, but also applicable to other surveillance data.}
\value{
  \item{areaID}{The area unique ID such as FIPS code and ZIP code will be added to the original data.}
}
\examples{
set.seed(2018)
dataset=data.frame(Patient=1:2,lat=rnorm(2,42,0.5),long=rnorm(2,-76,1))
\donttest{data.out=FIPS.name(data=dataset,ID.case="Patient",long.case="long",
lat.case="lat",state.map="36",level.map="tract",areaID="GEOID")
}
}
