#' Create a value box with a sparkline
#'
#' Modified version of [bs4Dash::valueBox()]
#'
#' @param data (optional) data.table to summarize with at least columns `x` and `y`
#' @param unit display unit
#' @param selected selected `x` value to highlight
#' @param info tooltip information text
#' @param sparkobj (optional) highcharts object
#' @param type (optional) type of spark chart (line, column, etc.) default to NA (no sparkline)
#'   if not passed
#' @inheritParams bs4Dash::valueBox
#'
#' @return
#' @importFrom scales comma
#' @import htmltools
#'
#' @examples
#' data <- data.table(
#'   x = Sys.Date() + 0:9,
#'   y = rnorm(10)
#' )
#' valueBoxSpark(data, title="Title", subtitle="Subtitle", color="success", type="line")
#'
#' data <- DATA[iso3=="ken" & id=="net_inflow" & period=="year", .(year, value)]
#' valueBoxSpark(data,
#'   title="Title", subtitle="Subtitle",
#'   color="success", type="bar", unit="km3")
#'
#' valueBoxSpark(33,
#'   title="Title", subtitle="Subtitle",
#'   color="success", unit="km3", footer="Footer")
#'
#' @export
valueBoxSpark <- function(
  value,
  unit = "",
  selected = NA,
  title = "",
  subtitle = "",
  info = NULL,
  sparkobj = NULL,
  type = NA,
  icon = NULL,
  color = "light",
  width = 4,
  href = NULL
){

  bg = NULL
  dt = NULL

  if(inherits(value, "data.frame")) {
    data = setnames(value, 1:2, c("x", "y"))

    # Infer periodicity
    freq = if(inherits(data$x, "Date")) data[, .N, by=year(x)][1, N] else 1
    freq = switch(as.character(freq), `1`="year", `2`="season", `12`="month", `36`="dekad")

    dt = data[, .(
      value = mean(fifelse(!is.na(selected) & x==selected, y, NA_real_), na.rm=T),
      mean = mean(y, na.rm=T),
      min = min(y, na.rm=T),
      max = max(y, na.rm=T),
      trend = lm(y~x, dt)$coefficients[["x"]],
      pct = mean(c(diff(y), NA)/y, na.rm=T)
    )]

    value = dt$value
    arr = c("&darr;", "&uarr;")[1 + (dt$trend >= 0)]
    status = c("orange", "green")[1 + (dt$trend >= 0)]
    bg = c("warning", "success")[1 + (dt$trend >= 0)]
    subtitle = if(missing(subtitle)) sprintf('%s %s per %s',
      arr, percent(dt$pct, acc=0.1), freq
    )

    sparkobj = if(!is.na(type)) hchart(data, type,
      hcaes(x, y), color=pal[[status]], name=NA) %>%
      hc_size(height=80) %>%
      hc_tooltip(pointFormat="{point.y:,.2f}") %>%
      hc_themed_vb()
    else sparkobj
  }

  info_icon = icon("info-circle",
    class="float-right text-info", `data-toggle`="tooltip", title=info)

  boxContent = div(class=sprintf("small-box bg-%s", color),
    div(class="inner",
      div(class="p-2",
        if(!missing(info)) info_icon,
        if(!missing(title)) p(class="lead text-bold", title),
        if(!missing(selected)) span(class="text-info", selected),
        if(!is.na(value)) h3(class="mt-0", comma(value, suffix=unit)),
        if(!is.null(dt)) tagList(
          span(class="text-info", "Average over period"),
          h3(class="mt-0", comma(dt$mean, suffix=unit)),
          span(class="text-info", "Min. / Max."),
          h5(class="mt-0", comma(dt$min, suffix=unit), "/", comma(dt$max, suffix=unit)),
          span(class="text-info", "Trend"),
          h5(class="mt-0", comma(dt$trend, suffix=paste(unit, "/", freq), acc=0.01))
        )
      ),
      if(!is.null(icon)) div(class="icon", icon),
      if(!is.null(sparkobj)) sparkobj
    ),
    div(class=sprintf("small-box-footer px-1 bg-%s text-center lead", bg),
      HTML(subtitle))
  )

  if(!is.null(href))
    boxContent = a(href=href, boxContent)

  div(class=if(!is.null(width)) sprintf("col-sm-%s", width),
    boxContent
  )
}


