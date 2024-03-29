% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highcharts.R
\name{plot_idx_mosaic}
\alias{plot_idx_mosaic}
\title{Plot temperature indices (highcharts)}
\usage{
plot_idx_mosaic(data, title = NA, subtitle = NA, ...)
}
\arguments{
\item{data}{output of \link{wc_trg*}}

\item{title}{chart title}

\item{subtitle}{chart subtitle}

\item{...}{
  Arguments passed on to \code{\link[=hc_themed]{hc_themed}}
  \describe{
    \item{\code{hc}}{a highchart object}
    \item{\code{label}}{optional annotation}
    \item{\code{base_font}}{default font for all chart elements}
    \item{\code{axes}}{show axes (default: TRUE)}
    \item{\code{exporting}}{include chart export menu (default: TRUE)}
    \item{\code{credits}}{include credits (default: FALSE)}
  }}
}
\description{
Plot temperature indices (highcharts)
}
\examples{
data <- data.table(
  year = rep(2000:2010, each=10),
  loc_id = 1:10,
  idx = rnorm(10, mean=80)
)
plot_idx_mosaic(data)

}
