\name{mediationking}
\alias{mediationking}
\title{Mediating Analysis}
\description{This function provides convenient algorithm to calculate total effect, mediation effect, direct effect and the proportion of mediation effect.}
\usage{mediationking(dataset,outcome,mediator,exposure,n.sim)}
\arguments{
  \item{dataset}{The dataset that is used for analysis.}
  \item{outcome}{The name of the outcome variable in the dataset.}
  \item{mediator}{The name of the mediator in the dataset.}
  \item{exposure}{The name of the exposure factor in the dataset.}
  \item{n.sim}{Times of simulation to estimate 95\% confidence intervals.}
  }
\details{Please use set.seed() if you want to get a consistent result; this function will be expended to allow more covariates shortly.}
\value{
  \item{Total effect}{The total effect of the exposure on the outcome variable.}
  \item{Indirect effect}{The effect of the exposure on the  outcome variable that is caused by mediator.}
  \item{Direct effect}{The effect of the exposure on the outcome variable that is caused by factors other than the mediator.}
  \item{Meditation.proportion}{The proportion of the mediation effect.}

}
\examples{
set.seed(1)
exposure<-rnorm(20,0,1)
mediator<-rnorm(20,10,1)
outcome<-rnorm(20,10,1)
dataset<-data.frame(outcome,mediator,exposure)
\donttest{mediationking(dataset,"outcome","mediator","exposure")
}
}
