\name{DBFgeocode}
\alias{DBFgeocode}
\title{Create a dbf File for Geocoding}
\description{Generate address variables and output the data as a dbf file for geocoding in ArcGIS.}
\usage{DBFgeocode(data,cityname,roadaddress,mailbox,ZIP)}
\arguments{
  \item{data}{A data.frame containing address variables that are necessry for geocoding.}
  \item{cityname}{The name of the variable in the data indicating city or county names.}
  \item{roadaddress}{The name of the variable in the data indicating home addresses.}
  \item{mailbox}{Optional address information such as the number of mailbox and the number of floor.}
  \item{ZIP}{The name of the variable in the data indicating ZIP codes.}
}
\value{Users may output the function return to the computer as the dbf file using write.dbf ().}
\note{In the dbf file, a variable named "singleline" will be used in the second step of geocoding, while variables roadaddress,cityname and ZIP will be seperately used in the first step, and the variable ZIP for the last step.}
\examples{
# similated data
datatest=data.frame(county=c("Albany","Albany","Albany"),
                address1=c("1 Lincoln ave","2 Lincoln ave","489 Washinton ave"),
                address2=c("1st floor","1st floor","2nd floor"),
                zip=12206
                )
DBFgeocode(data=datatest,cityname="county",roadaddress="address1",
mailbox="address2",ZIP="zip")
}
