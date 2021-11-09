#' Plot variable time-series
#'
#' @param dt
#' @param color
#'
#' @return
#' @export
#'
#' @examples
plot_ts <- function(dt, color) {

  unit = " km³"

  highchart() %>%
    hc_chart(zoomType="x") %>%
    hc_add_series(dt, type="area",
      hcaes(x=year(year), y=value), name=dt[1, variable],
      color=color, fillColor=alpha(color, .2), marker=list(enabled=TRUE)) %>%

    hc_tooltip(valueSuffix=unit,
      pointFormat="{series.name}: <strong>{point.y:,.2f}</strong><br/>") %>%
    hc_legend(enabled=TRUE, align="top") %>%
    hc_themed()
}
