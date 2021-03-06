% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcPSG-ind-log.R
\name{calcPSG.ind.log}
\alias{calcPSG.ind.log}
\title{Calculate plume direction and spread using indiviudal data and a lognormal fit}
\usage{
calcPSG.ind.log(dat, plot = TRUE)
}
\arguments{
\item{dat}{Data table, with names obtained from GMAP data output as of 2018-Aug}
}
\value{
object of class "lm" with plume fit
}
\description{
Calculate plume direction and spread using indiviudal data and a lognormal fit
}
\examples{
calcPSG.ind.log(dat)
}
