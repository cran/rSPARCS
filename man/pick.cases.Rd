\name{pick.cases}
\alias{pick.cases}
\title{Select cases within certain distance around a site}
\description{Identify the closest site (e.g. monitoring sites) for each case, and select cases within certain distance around a site, e.g. 15 miles buffer.}
\usage{pick.cases(data,long.case,lat.case,long.sites,lat.sites,radius)}
\arguments{
  \item{data}{a data.frame containing the coordinates of cases.}
  \item{long.case}{the name of variable in the data indicating the longitude of cases.}
  \item{lat.case}{the name of variable in the data indicating the latitude of cases.}
  \item{long.sites}{a numeric vector containing the longitude of sites.}
  \item{lat.sites}{a numeric vector containing the latitude of sites.}
  \item{radius}{radius of the buffer, e.g."15 miles", "20 kms".}
}
\details{Not limited to hospital data, but also applicable to other surveillance data.}
\value{
  \item{which.site}{the closest site to the case.}
  \item{minDIST}{the distance of the case to the closest site; in the same unit as "radius".}
  \item{Select}{an indicator of whether a case was within the buffer.}
}
\references{
Zhang W, Lin S, Hopke PK, et al. Triggering of cardiovascular hospital admissions by fine particle concentrations in New York state: Before, during, and after implementation of multiple environmental policies and a recession. Environ. Pollut. [electronic article]. 2018;242:1404–1416.
}
\examples{
set.seed(2018)
data=data.frame(Patient=1:100,lat=rnorm(100,41,0.5),long=rnorm(100,-76,1))

long.monitor=c(-73.75464,-78.80953,-73.902,-73.82153,-77.54817)
lat.monitor=c(42.64225,42.87691,40.81618,40.73614,43.14618)

data.out=pick.cases(data,long.case="long",lat.case="lat",
long.sites=long.monitor,lat.sites=lat.monitor,radius="30 miles")
data.out
}
