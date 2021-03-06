% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/highcharts.R
\name{plot_idx}
\alias{plot_idx}
\title{Plot yearly index time series with EMA trends (highcharts)}
\usage{
plot_idx(
  data,
  title = "Index Trend",
  subtitle = "Average across portfolio locations with EMA trend, 1990-2020 (mm)",
  label = NULL,
  rowheight = 280,
  ncol = 1,
  ...
)
}
\arguments{
\item{data}{output of \link{wc_trg_*}, see examples}

\item{title}{chart title}

\item{subtitle}{chart subtitle}

\item{label}{optional annotation}

\item{rowheight}{Height in px.}

\item{ncol}{how many columns in the grid}

\item{...}{
  Arguments passed on to \code{\link[=hc_themed]{hc_themed}}
  \describe{
    \item{\code{hc}}{a highchart object}
    \item{\code{base_font}}{default font for all chart elements}
    \item{\code{axes}}{show axes (default: TRUE)}
    \item{\code{exporting}}{include chart export menu (default: TRUE)}
    \item{\code{credits}}{include credits (default: FALSE)}
  }}
}
\description{
Plot yearly index time series with EMA trends (highcharts)
}
\examples{
dt <- data.table(
 loc_id = 1:3,
 period = rep(c("prd_1", "prd_5"), 3),
 year = rep(1990:2020, each=3*2),
 idx = rnorm(31*3*2, 10),
 idx_lo = 8,
 idx_hi = 12,
 ecc = rnorm(31*3*2, .5)
)
plot_idx(dt)

}
