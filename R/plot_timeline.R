#' Plot monthly basin time series (highcharts)
#'
#' @param data biophysical time-series
#' @param sos start of season threshold (default: 35 mm or 8°C Tmin for blooming)
#' @param yrange y-axis min and max
#' @inheritParams hc_themed
#' @import highcharter data.table
#' @importFrom lubridate ceiling_date year<-
#' @examples
#'
#' @export
plot_timeline <- function(
  data,
  yrange = NULL,
  title = NULL,
  subtitle = NULL,
  unit = "km³",
  ...) {

  # Add trend decomposition
  ts = ts(data[, value], start=c(data[, min(year(date))], 1), frequency=12)
  tr = stl(ts, "periodic")
  data[, trend := tr[[1]][, "trend"]]

  yrange = if(missing(yrange)) data[, c(min(value, na.rm=T), max(value, na.rm=T))] else yrange

  xBands = lapply(data[, seq(min(date), max(date), by="year")], function(x) list(
    from = datetime_to_timestamp(as.Date(x)),
    to = datetime_to_timestamp(as.Date(x)+365-1),
    color = c("transparent", alpha(pal[["light"]], .4))[1 + odd(year(x))]
  ))

  p = highchart(type="stock") %>%
    hc_add_series(dt, type="arearange", name="Min./Max.",
      hcaes(x=date, low=ymin, high=ymax),
      color=pal[[1]], lineWidth=0, marker=list(enabled=FALSE)) %>%
    hc_add_series(dt, type="line", name=unit, hcaes(x=date, y=value),
      color=pal[[1]], lineWidth=.5) %>%
    hc_add_series(dt, type="line", name="LTN", hcaes(x=date, y=ltn),
      color=pal[[1]], lineWidth=2, marker=list(symbol="circle")) %>%
    hc_add_series(dt, type="line", name="Trend", hcaes(x=date, y=trend),
      color=pal[["orange"]], lineWidth=2, marker=list(symbol="circle")) %>%

    hc_legend(enabled=TRUE, align="right") %>%
    hc_tooltip(valueSuffix=unit, shared=TRUE) %>%
    hc_xAxis(type="datetime", plotBands=xBands) %>%
    hc_yAxis(min=yrange[1], max=yrange[2]) %>%
    hc_rangeSelector(enabled=TRUE, inputEnabled=FALSE) %>%
    hc_navigator(enabled=FALSE) %>%
    hc_scrollbar(enabled=FALSE) %>%
    hc_themed(title, subtitle, ...)

  return(p)
}
