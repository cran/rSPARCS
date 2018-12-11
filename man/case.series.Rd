\name{case.series}
\alias{case.series}
\title{Generate the Case Series}
\description{Estimates the daily number of cases reported by multiple grouping factors.}
\usage{
case.series(data,ICD,diagnosis,date,start,end,by1,by2,by3,by4,by5)
}
\arguments{
  \item{data}{a data.frame containing with each row representing a case, and each column representing the patient characteristics such as gender, age, admission date, and discharge date, etc.}
  \item{ICD}{a vector of ICD 9, or 10 codes, or a mix of them, which users are willing to calculate the daily numbers for; can be of length 3-6.}
  \item{diagnosis}{the name of the variable in the data containing the diagnostic code upon admission.}
  \item{date}{the name of the variable in the data showing the admission date, either in the format like "20181129" or "2018/11/29".}
  \item{start,end}{the start and end date for the case series to be generated.}
  \item{by1,by2,by3,by4,by5}{the name of the variable in the data used as grouping variables.}
}
\details{Not limited to hospital data, but also applicable to other surveillance data.}
\value{
  \item{dataset}{A case series will be generated for time series analysis, trend analysis and displaying, with following variables:}
  \item{date}{from the start date to the end date as user specified, with 1 day bin.}
  \item{case}{the daily number of cases diagnosed with diseases of user specified ICD codes.}
  \item{others}{grouping variables.}
}
\note{
  When applied to other medical data without ICD code, users may arbitrarily set a ICD code, meanwhile, define the diagnosis variable in the data to the same ICD code.
}
\examples{
# similated data
set.seed(2018)

data=data.frame(
  patient=1:10000,
  primdiag=sample(390:398,10000,replace=TRUE),
  onset=sample(seq.Date(as.Date("2015/2/1"),
    as.Date("2016/2/1"),"1 day"),10000,replace=TRUE),
  sex=sample(c("M","F"),10000,replace=TRUE),
  county=sample(c("Albany","New York"),10000,replace=TRUE)
)

output.series=case.series(
  data,ICD=392:396,diagnosis="primdiag",
  date="onset",start="2015/1/1",end="2016/12/31",by1="sex")
  
head(output.series)
}
