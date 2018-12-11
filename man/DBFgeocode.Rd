\name{DBFgeocode}
\alias{DBFgeocode}
\title{Create a dbf File for Geocoding}
\description{Generate address variables and output the data as a dbf file for geocoding.}
\usage{DBFgeocode(data,cityname,roadaddress,mailbox,ZIP,output)}
\arguments{
  \item{data}{a data.frame containing address variables that are necessry for geocoding.}
  \item{cityname}{the name of the variable in the data indicating city or county names.}
  \item{roadaddress}{the name of the variable in the data indicating home addresses.}
  \item{mailbox}{optional address information such as the number of mailbox and the number of floor.}
  \item{ZIP}{the name of the variable in the data indicating ZIP codes.}
  \item{output}{specify the name of the dbf file (.dbf) and the directory for output.}
}
\details{The suffix of the output argument should be ".csv" instead of ".dbf".}
\value{A dbf file will be automatically output to the computer.}
\note{In the dbf file, a variable named "singleline" will be used in the second step of geocoding, while variables roadaddress,cityname and ZIP will be seperately used in the first step, and the variable ZIP for the last step.}
\examples{
# similated data
datatest=data.frame(county=c("Albany","Albany","Albany"),
                address1=c("1 Lincoln ave","2 Lincoln ave","489 Washinton ave"),
                address2=c("1st floor","1st floor","2nd floor"),
                zip=12206
                )
DBFgeocode(data=datatest,cityname="county",roadaddress="address1",
mailbox="address2",ZIP="zip",output="data_output.csv")
}
