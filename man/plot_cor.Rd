% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highcharts.R
\name{plot_cor}
\alias{plot_cor}
\title{Plot correlation matrix (highcharts)}
\usage{
plot_cor(data, title = NA, subtitle = NA, ...)
}
\arguments{
\item{data}{output of \link{wc_ecc}}

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
Plot correlation matrix (highcharts)
}
\examples{
data <- data.table(
  ecc_grm = rnorm(20),
  ecc_cdd = rnorm(20),
  ecc_crf = rnorm(20),
  ecc_crd = rnorm(20),
  ecc_crw = rnorm(20)
)
plot_cor(data)

}
