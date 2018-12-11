\name{CXover.data}
\alias{CXover.data}
\title{Generate the Dataset for Case Crossover Analysis}
\description{Generate the dataset for case crossover analysis.}
\usage{CXover.data(data,date,ID,direction)}
\arguments{
  \item{data}{a data.frame containing the date of each case.}
  \item{date}{the name of the variable in the data indicating the date of each case reported to the database.}
  \item{ID}{the name of the variable in the data indicating case ID, if not specified, it will automatically generated starting from 1.}
  \item{direction}{"pre4"or"month4". With "pre4", each case day will be matched with same weekdays in previous 4 weeks. With "month4", each case day will be matched with same weekdays in the same month.}
}
\details{Not limited to hospital data, but also applicable to other surveillance data.}
\value{
  \item{dataset}{A data.frame ready for the case crossover analysis, with following variables:}
  \item{ID}{same ID represents the same patient.}
  \item{Date}{one case day is matched with 3-4 control days.}
  \item{status}{indicating whether it is a case day or a control day.}
}
\references{
Zhang W, Lin S, Hopke PK, et al. Triggering of cardiovascular hospital admissions by fine particle concentrations in New York state: Before, during, and after implementation of multiple environmental policies and a recession. Environ. Pollut. 2018;242:1404–1416.
}
\author{
Wang-Jian Zhang (wzhang27@albany.edu)
}
\examples{
# similated data
set.seed(2018)
dataset=data.frame(
  patient=1:1000,
  primdiag=sample(390:398,1000,replace=TRUE),
  onset=sample(seq.Date(as.Date("2015/2/1"),as.Date("2016/2/1"),"1 day"),1000,replace=TRUE),
  sex=sample(c("M","F"),1000,replace=TRUE),
  county=sample(c("Albany","New York"),1000,replace=TRUE))

out.data=CXover.data(data=dataset,date="onset",ID="patient",direction="pre4")
head(out.data)
}
