% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{DATA}
\alias{DATA}
\title{Modeled WA+ data cube}
\format{
A data.table with columns:
\itemize{
\item iso3
\item sheet
\item period
\item date_start
\item date_end
\item year
\item month
\item unit
\item id
\item value
}
}
\usage{
DATA
}
\description{
A data cube of WA+ hydrological time-series generated by \link{data_etl}. Field codes
are documented in \link{META}.
}
\examples{
names(DATA)
DATA[1:5]

}
\seealso{
\link{META}
}
\keyword{datasets}
