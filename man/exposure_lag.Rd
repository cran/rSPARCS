\name{exposure_lag}
\alias{exposure_lag}
\title{
Calculate Individual and Cumulative Lags for Exposure
}
\description{
Calculate individual and cumulative lag exposure for specific variables. Cumulative lag exposure was calculated by using moving average.
}
\usage{
exposure_lag(data,var,maxlag,ID,Date,lag_suffix)
}
\arguments{
  \item{data}{A dataframe.}
  \item{var}{Variable names in the dataframe to specify variables to be used for the lag calculation.}
  \item{maxlag}{A number. The max day for calculating the lag exposure.}
  \item{ID}{A variable name. The exposure station ID.}
  \item{Date}{A variable name. A variable indicating the date of exposure measurement.}
  \item{lag_suffix}{A two-length vector indicating the cumulative lag or the individual lag. The first was the suffix for cumulative lag exposure. The second was for individual lag exposure. Default: c('_cu_lag','_si_lag')}
}

\value{
It returns a dataframe with calculated individual and cumulative lag exposures. 'var_cu_lag5' means the moving average from lag 0 to lag 5 days. 'var_si_lag5' means the exposure 5 days ago.
}
\references{
Deng X, Friedman S, Ryan I, et al. The independent and synergistic impacts of power outages and floods on hospital admissions for multiple diseases [published online ahead of print, 2022 Mar 5]. Sci Total Environ. 2022;828:154305. doi:10.1016/j.scitotenv.2022.154305
}
\examples{
data=data.frame(
  ID=rep(1:5,each=5),
  Date=seq(as.Date('2022-01-01'),as.Date('2022-01-05'),by='1 day'),
  x=rnorm(25)
)

exposure_lag(data,var='x',maxlag=3,ID='ID',Date='Date')
}
