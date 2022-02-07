#' Plot radar charts
#'
#' @param iso3
#' @inheritDotParams hc_themed
#'
#' @examples
#' plot_radar(iso3="ken")
#'
#' @export
plot_radar <- function(iso3=names(ISO3), ...) {

  iso = match.arg(iso3)
  unit = ISO3[[iso]][["unit"]]

  dt = DATA[iso3==iso & period=="year"
    & id %in% c("agriculture", "net_inflow", "outflow", "depleted_water", "available_water"),
    .(id=str_replace(id, "_", " "), year, value)][order(id, year)]

  prd = dt[, paste(range(year), collapse="-")]

  dt1 = dt[, .(
    value = mean( (value-shift(value))/value, na.rm=T)
  ), keyby=id]

  dt2 = dt[, .(
    year = year,
    color = pal[["black"]],
    value = (value-shift(value))/value
  ), keyby=.(id)]

  highchart() %>%
    hc_chart(height="110%", polar=TRUE) %>%

    hc_add_series(dt2, type="line",
      hcaes(x=toupper(id), y=100*value, group=year, name=year),
      pointInterval=360/5, lineWidth=1, color=alpha(pal[["navy"]], .9),
      marker=list(enabled=FALSE)) %>%

    hc_add_series(dt1, type="line",
      hcaes(x=toupper(id), y=100*value), name="Average",
      pointInterval=360/5, lineWidth=6, color=pal[["blue"]],
      marker=list(enabled=TRUE, size=2)) %>%

    hc_xAxis(type="category",
      labels=list(fontSize="16px",
        format='<span class="font-weight-bold">{value}</span>')) %>%

    hc_yAxis(tickInterval=20,
      labels=list(format="{value} %"),
      plotLines=list(
        list(value=0, color=pal[["red"]], width=1)),
      plotBands=list(
        list(from=-120, to=0, color=alpha(pal[["red"]], .1))
      )
    ) %>%

    hc_tooltip(pointFormat=
        '<span class="pradar lead">{series.name} {point.y:,.0f}%</span><br/>',
      borderWidth=0, backgroundColor=pal[["light"]], shared=FALSE) %>%
    hc_legend(enabled=FALSE) %>%
    hc_themed(
      title = "System Trends",
      subtitle = prd)

}
