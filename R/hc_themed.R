#' Custom Highcharts theme
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
#'
#' @examples
#' data <- data.table(
#'   x = Sys.Date() + 0:9,
#'   y = rnorm(10)
#' )
#'
#' hchart(data, "line", hcaes(x, y)) %>%
#'   hc_themed()
#'
#' @export
hc_themed <- function(
    hc,
  title = NULL,
  subtitle = NULL,
  label = NULL,
  x = NULL,
  y = NULL,
  base_font = "national-web-book",
  axes = TRUE,
  exporting = TRUE,
  credits = FALSE,
  ...) {

  thm = hc_theme(

    chart = list(
      style = list(fontFamily=base_font),
      backgroundColor = "transparent"
    ),
    # Don't use semantic colors
    colors = unname(pal[names(pal)!="red"]),
    title = list(
      style = list(color=pal[["black"]], fontSize="18px"),
      align = "left"
    ),
    subtitle = list(
      style = list(color=pal[["black"]], fontSize="15px"),
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
      borderWidth=0, backgroundColor=alpha(pal[["light"]], .85), shadow=FALSE,
      xDateFormat = "%Y-%m-%d", dateTimeLabelFormats = "%Y-%m-%d", valueDecimals = 1,
      style = list(color=pal[["black"]]), backgroundColor = pal[["light"]]
    ),
    plotOptions = list(
      series = list(
        opacity = .8,
        connectNulls = TRUE,
        dataLabels = list(
          enabled = NA, shadow = FALSE, align="left",
          style = list(color=pal[["black"]], fontSize="15px", fontWeight="normal")
        )
      ),
      area = list(
        lineWidth = 0,
        fillOpacity = .3,
        marker = list(enabled=FALSE, radius=0)
      ),
      arearange = list(
        lineWidth = 0,
        fillOpacity = .2,
        marker = list(enabled=FALSE, radius=3, symbol="circle")
      ),
      pie = list(
        borderWidth = 1, borderColor = "#fff",
        startAngle = -90, endAngle = 270,
        size = "84%", innerSize = "68%"
      ),
      bullet = list(
        pointPadding = 0.25, borderWidth = 1,
        targetOptions = list(width="200%", color=pal[["orange"]])
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
      ),
      solidgauge = list(
        borderWidth = 1, borderColor = "#fff",
        dataLabels = list(enabled=TRUE, pointFormat="{series.name}"),
        stickyTracking=FALSE
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
    )
  )

  if (length(list(...)) > 0) thm = hc_theme_merge(thm, hc_theme(...))

  p = hc_add_theme(hc, thm) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle)

  if(length(label) > 0) p = p %>%
    hc_annotations(list(labels = list(
      list(text=label, useHTML=TRUE, shape="rect", point=list(x=x, y=y)))))

  return(p)
}


#' Custom Highcharts theme for sparklines
#'
#' This theme is used inside value boxes.
#'
#' @param hc a highchart object
#' @inheritDotParams highcharter::hc_theme
#'
#' @examples
#' data <- data.table(
#'   x = Sys.Date() + 0:9,
#'   y = rnorm(10)
#' )
#'
#' hchart(data, "line", hcaes(x, y)) %>%
#'   hc_themed_vb() %>%
#'   hc_chart(backgroundColor=pal[["red"]])
#'
#' @export
hc_themed_vb <- function(hc, ...) {

  thm = hc_theme(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        opacity = .6,
        fillOpacity = .25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    tooltip = list(
      xDateFormat = "%Y-%m-%d",
      dateTimeLabelFormats = "%Y-%m-%d",
      valueDecimals = 1
    ),
    exporting = list(enabled = FALSE),
    credits = list(enabled = FALSE, text = "")
  )

  if (length(list(...)) > 0) thm = hc_theme_merge(thm, hc_theme(...))
  p = hc_add_theme(hc, thm)
  return(p)
}
