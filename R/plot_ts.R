#' Plot variable time-series
#'
#' @param dt
#' @param color
#' @param unit
#'
#' @return
#'
#' @examples
#' plot_ts(DATA[iso3=="mli" & sheet=="sheet1" & variable=="Rainfall"])
#'
#' @export
plot_ts <- function(dt, color=pal[1], unit="km³") {

  unit = paste("", unit)

  highchart() %>%
    hc_chart(zoomType="x") %>%
    hc_add_series(dt, type="area",
      hcaes(x=date_end, y=value), name=dt[1, variable],
      color=color, fillColor=alpha(color, .2), marker=list(enabled=TRUE)) %>%

    hc_xAxis(type="datetime", dateTimeLabelFormats=list(month="%e %b")) %>%
    hc_tooltip(valueSuffix=unit,
      pointFormat="{series.name}: <strong>{point.y:,.2f}</strong><br/>") %>%
    hc_legend(enabled=TRUE, align="top") %>%
    hc_themed()
}
