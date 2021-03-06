% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ts.R
\name{plot_tss}
\alias{plot_tss}
\title{Plot simple variable time-series (highcharts)}
\usage{
plot_tss(data, color = pal[[1]], unit = NA)
}
\arguments{
\item{data}{data.table to plot with columns \code{date} and \code{value} (will use \code{date_end}
if \code{date} is missing)}

\item{color}{an optional built-in color name (default "navy")}

\item{unit}{display unit}
}
\value{

}
\description{
Plot simple variable time-series (highcharts)
}
\examples{
plot_tss(DATA[iso3=="mli" & sheet=="sheet1" & id=="rainfall"])

}
