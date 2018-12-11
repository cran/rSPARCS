\name{FIPS.name}
\alias{FIPS.name}
\title{Add county/census tract names or FIPS code}
\description{Identify the residential census tracts for each case, and add county/census tract names or FIPS code.}
\usage{FIPS.name(data,patco,level,add,addfrom,state,county,map,long.case,lat.case,censusFIPS)}
\arguments{
  \item{data}{a data.frame containing coordinates of cases for level="census"}
  \item{patco}{the name of variable in the data indicating the county code.}
  \item{level}{"county" or "census", indicating the study level.}
  \item{add}{"name" or "FIPS", or a vector containing both, to specify what vairables to be added.}
  \item{addfrom}{a data.frame containing "COUNTY" (county names),"FIPS" (county FIPS code) and"CODE" (county code, should match those in the health data).} 
  \item{state}{State FIPS code, e,g, "36" for the New York State.}
  \item{county}{County FIPS code, e,g, "36001" for Albany, we use "001" for Albany here.}
  \item{map}{A map for a region outside the U.S. can also be imported as a "spatialpolygonsdataframe" object.}
  \item{long.case}{the name of variable in the data indicating the longitude of cases.}
  \item{lat.case}{the name of variable in the data indicating the latitude of cases.}
  \item{censusFIPS}{the name of variable in the map indicating the FIPS for census tracts. Use the default if the study is within the U.S.}
}
\note{If you are working on the NY SPARCS data,no "addfrom" is required as this package has include a public information from https://www.health.ny.gov/statistics/sparcs/sysdoc/appf.htm
}
\details{Not limited to hospital data, but also applicable to other surveillance data.}
\value{
  \item{FIPS}{the FIPS code at county or census tract level depending on the arguement "level".}
  \item{county}{the name of counties.}
}
\author{
Wang-Jian Zhang (wzhang27@albany.edu)
}
\examples{
dataset=data.frame(Patient=1:10,county=5:14)
data.out=FIPS.name(data=dataset,patco="county",level="county")

#set.seed(2018)
#dataset=data.frame(Patient=1:10,lat=rnorm(10,42,0.5),long=rnorm(10,-76,1))
#data.out=FIPS.name(data=dataset,level="census",state="36",
#long.case="long",lat.case="lat",censusFIPS="GEOID")

head(data.out)
}
