\name{desc.comp}
\alias{desc.comp}
\title{Generate a Descriptive Table}
\description{Generate a comprehensive descriptive table with intergroup comparison.}
\usage{
desc.comp(data,variables,by,margin,avg.num,test.num)
}
\arguments{
  \item{data}{a data.frame containing the variables to be described and a group variable}
  \item{variables}{a numeric variable indicating the columns of variables to be described.}
  \item{by}{a number indicating the column of the group variable}
  \item{margin}{calculate the proportion for categorical variables by 1 (row) or 2 (column).}
  \item{avg.num}{"mean", describe continuous variables with mean and standard deviation; "median", describe continuous variables with median and interquantile range; otherwise, normal distribution test will be conducted, for normal distributed variables, "mean" will be used, otherwise, "median" will be used.}
  \item{test.num}{"metric", t test or anova will be used for intergroup comparison; "nonmetric", Wilcoxon rank sum test or Kruskal-Wallis test will be used; otherwise, normal distribution test will be conducted, for normal distributed variables, "metric" will be used, otherwise, "nonmetric" will be used.}
}
\details{Not limited to the SPARCS, but also applicable to other research data.}
\value{
A comprehensive descriptive table with statistics and P value for intergroup comparisons.
}
\author{
Wang-Jian Zhang (wzhang27@albany.edu)
}
\examples{
desc.comp(CO2,variables=2:5,by=1,margin=1)
}
