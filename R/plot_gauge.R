#' Plot radar charts
#'
#' @param iso3
#' @inheritDotParams hc_themed
#'
#' @examples
#' plot_gauge(iso3="ken")
#'
#' @export
plot_gauge <- function(iso3=names(ISO3), unit="km3", ...) {

  iso = match.arg(iso3)
  unit = paste("", unit)

  dt = DATA[iso3==iso & period=="year"
    & id %in% c("agriculture", "economy", "energy", "environment",
      "leisure", "net_inflow", "outflow")
  ]

  prd = dt[, paste(range(year), collapse="-")]

  dt = dt[, .(value = mean(value, na.rm=T)), by=.(id)
  ][, group := fcase(
    id=="agriculture", "agriculture",
    id=="net_inflow", "inflow",
    id=="outflow", "outflow",
    id=="leisure", "non-agriculture",
    id=="economy", "non-agriculture",
    id=="energy", "non-agriculture",
    id=="non_beneficial", "non-agriculture",
    id=="environment", "non-agriculture"
  )][, .(value=sum(value, na.rm=T)), keyby=.(group)
  ][c(2,3,1,4)]

  dt[, `:=`(
    pct = 100*value/dt[1, value],
    color = pal[1:4],
    radius = c("112%", "86%", "61%", "36%"),
    innerRadius = c("88%", "63%", "38%", "13%")
  )]

  highchart() %>%
    hc_chart(height="100%") %>%
    hc_pane(startAngle=0, endAngle=180,
      background=list(backgroundColor=alpha(pal[["light"]], .1))
      # background=list(
      #   list(
      #     outerRadius="112%", innerRadius="89%", borderWidth=0,
      #     backgroundColor=alpha(pal[[1]], .1)),
      #   list(
      #     outerRadius="86%", innerRadius="63%", borderWidth=0,
      #     backgroundColor=alpha(pal[[2]], .1)),
      #   list(
      #     outerRadius="61%", innerRadius="38%", borderWidth=0,
      #     backgroundColor=alpha(pal[[3]], .1)),
      #   list(
      #     outerRadius="36%", innerRadius="13%", borderWidth=0,
      #     backgroundColor=alpha(pal[[4]], .1))
      # )
    ) %>%

    hc_add_series(dt, type="solidgauge",
      hcaes(y=pct, color=color, radius=radius, innerRadius=innerRadius, group=group),
      borderWidth=1, borderColor="#fff",
      marker=list(enabled=TRUE)) %>%

    hc_yAxis(min=0, max=100, lineWidth=0, tickPositions=list()) %>%
    hc_tooltip(
      pointFormat=
        '<span class="pr-3 lead">{series.name}<br/>
        <strong>{point.pct:,.0f}%</strong><br/>
      {point.value:,.2f} km³ / year</span>',
      borderWidth=0, backgroundColor=NA, shadow=FALSE) %>%
    hc_legend(enabled=FALSE) %>%
    hc_themed(
      title = "System Water Uses",
      subtitle = prd)

}

