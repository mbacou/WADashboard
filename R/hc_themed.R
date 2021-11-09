#' Custom HighCharts theme
#'
#' @param hc a highchart object
#' @param title chart title
#' @param subtitle chart subtitle
#' @param label optional annotation
#' @param base_font default font for all chart elements
#' @param axes show axes (default: TRUE)
#' @param exporting include chart export menu (default: TRUE)
#' @param credits include credits (default: FALSE)
#' @inheritDotParams highcharter::hc_theme
#' @import highcharter
#' @export
hc_themed <- function(
  hc,
  title = NULL,
  subtitle = NULL,
  label = NULL,
  x = NULL,
  y = NULL,
  base_font = getOption("wa.font"),
  axes = TRUE,
  exporting = TRUE,
  credits = FALSE,
  ...) {

  thm <- hc_theme(

    chart = list(
      style = list(fontFamily=base_font),
      backgroundColor = "transparent"
    ),
    # Don't use semantic colors
    colors = unname(pal[names(pal)!="red"]),
    title = list(
      style = list(color=pal[["black"]], fontSize="16px"),
      align = "left"
    ),
    subtitle = list(
      style = list(color=pal[["black"]], fontSize="13px"),
      align = "left"
    ),
    legend = list(
      enabled = TRUE,
      itemStyle = list(color=pal[["black"]]),
      verticalAlign = "top",
      align = "left",
      itemHoverStyle = list(color=pal[["black"]])
    ),
    xAxis = list(
      title = list(enabled=FALSE),
      dateTimeLabelFormats = list(day='%e %b', week='%e %b %y', month='%b-%y', year='%Y'))
    ,
    yAxis = list(
      title = list(enabled=FALSE)
    ),
    tooltip = list(
      enabled = TRUE, shared = TRUE, split = FALSE,
      pointFormat = "{series.name}: <strong>{point.y:,.1f}</strong><br/>",
      xDateFormat = "%Y-%m-%d",
      dateTimeLabelFormats = "%Y-%m-%d",
      valueDecimals = 2
    ),
    plotOptions = list(
      series = list(
        opacity = .8,
        connectNulls = TRUE,
        marker = list(enabled=NA, radius=3, enabledThreshold=8),
        dataLabels = list(enabled=NA, style=list(fontSize="11px"))
      ),
      area = list(
        lineWidth = 0,
        fillOpacity = .3,
        marker = list(enabled=FALSE, radius=0)
      ),
      arearange = list(
        lineWidth = 0,
        fillOpacity = .2,
        marker = list(enabled=FALSE, radius=3)
      ),
      pie = list(
        lineWidth = 0,
        startAngle = -90, endAngle = 270,
        size = "84%", innerSize = "68%", center = c("50%", "40%"),
        dataLabels = list(enabled=FALSE)
      ),
      bullet = list(
        pointPadding = 0.25, borderWidth = 0,
        targetOptions = list(width="200%", color=pal[["blue"]])
      ),
      boxplot = list(
        fillColor = alpha(pal[["black"]], .3),
        lineWidth = .5, whiskerWidth = 5, stemWidth = 1, stemDashStyle = "solid",
        whiskerColor = pal[["blue"]], stemColor = pal[["blue"]],
        medianColor = pal[["blue"]]
      ),
      heatmap = list(
        marker = list(enabled=TRUE, lineWidth=6, lineColor=pal[["light"]]),
        dataLabels = list(enabled=TRUE, pointFormat="{point.value:,.0f}")
      )
    ),
    exporting = list(
      enabled = exporting,
      csv = list(dateFormat="%Y-%m-%d"),
      buttons = list(contextButton=list(
        symbolSize = 12,
        symbolFill = pal[["light"]],
        symbolStroke = pal[["black"]],
        symbolStrokeWidth = 1.3,
        menuItems = c("printChart", "downloadPNG", "downloadSVG", "downloadCSV")))
    ),
    credits = list(
      enabled = credits,
      position = list(align="left"),
      href = "https://wateraccounting.org/",
      style = list(fontSize="11px")
    ),
    ...
  )

  p  = hc_add_theme(hc, thm) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle)

  if(length(label) > 0) p = p %>%
    hc_annotations(list(labels = list(
      list(text=label, useHTML=TRUE, shape="rect", point=list(x=x, y=y)))))

  return(p)
}
