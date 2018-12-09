\name{dupl.readm}
\alias{dupl.readm}
\title{Identify Duplicates and Re-admissions}
\description{Identify the duplicates and re-admissions in the SPARCS or other research data with subject identifications.}
\usage{dupl.readm(data,UniqueID,date,period)}
\arguments{
  \item{data}{a data.frame containing "UniqueID" and "date"}
  \item{UniqueID}{the name of the variable in the data indicating case ID.}
  \item{date}{the name of the variable in the data indicating the admission/onset date.}
  \item{period}{the time period used to define an re-admission; period=365 by default.}
}
\details{Not limited to the SPARCS, but also applicable to other research data with "UniqueID" and "date".}
\value{
  \item{id.dupl}{indicating whether it is a duplicated record with exactly the same "UniqueID" and "date" as a previous record. In the SPARCS data,some patients may be reported twice or even more due to insurance issues. For most studies, researchers may remove this kind of duplicates to avoid potential overcounting problems.}
  \item{onlyone}{indicating whether this is the only record with this ID.}
  \item{Period}{the time period between the current visit and the previous one for a patient; 0 for the 1st visit; and NA for those with only one record.}
  \item{Nadmission}{indicating the times of admission, e.g. 1st, 2nd admission; a patient may have more than one 1st admissions if some periods between two visits are greater than e.g. 365 days.}
}
\author{
Wang-Jian Zhang (wzhang27@albany.edu)
}
\examples{
dataset=data.frame(
  ID=c(1,3,4,2,4,6,3,5,7,1),
  onset=c("2015/1/1","2016/1/2","2015/5/9",
          "2015/12/1","2016/8/2","2015/5/9",
		  "2015/11/1","2016/3/2","2016/5/9","2015/9/9")
)

out.data=dupl.readm(data=dataset,
                    UniqueID="ID",date="onset",period=365)
head(out.data)
}
