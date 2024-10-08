% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LS_script.R
\name{LS}
\alias{LS}
\title{LS periodogram}
\usage{
LS(df, unc, maxt = 10, N = 100)
}
\arguments{
\item{df}{dataframe}

\item{unc}{uncertain}

\item{maxt}{maximum T}

\item{N}{number on grid}
}
\value{
period estimate
}
\description{
LS periodogram
}
\examples{
t <- seq(from = 1, to = 10, length.out= 1000)
x <- 2*sin(2*pi*t/5+0.5)+ 1 + rnorm(length(t),0, 0.25)
df <- data.frame(t,x)
LS(df,unc=0.25,maxt=10,1000)
}
