#' Plot hydrological time-series (highcharts)
#'
#' @param data data.table to plot with columns `date` and `value` (will use `date_end`
#'   if `date` is missing)
#' @param name series' display name
#' @param unit display unit
#' @param yrange manual y-axis range
#' @param unit.threshold maximum threshold to convert from km³ to MCM unit (default: 10 km³)
#' @inheritParams hc_themed
#' @inheritDotParams hc_themed
#'
#' @import data.table
#' @import highcharter
#' @importFrom scales comma percent
#' @importFrom gtools odd
#'
#' @return highcharter object
#'
#' @examples
#'
#' @export
plot_ts <- function(data, name=NA, unit=NA, yrange=NULL,
  unit.threshold=10, subtitle=NULL, ...) {

  dt = copy(data)
  if(!"date" %in% names(dt)) setnames(dt, "date_end", "date", skip_absent=TRUE)
  setorder(dt, date)

  if(dt[, .(unit)=="km³" & (max(value, na.rm=T) < unit.threshold)]) {
    dt[, value := 1000* value]
    unit = "MCM"
  }

  dt[, `:=`(
    # Running monthly mean / LTN
    ltn = cumsum(value)/1:.N
  ), by=month(date)]

  # Infer periodicity (1=year, 2=season, 12=month, 36=dekad)
  freq = dt[, .N, by=year(date)][1, N]

  tr = if(freq > 1) {
    # Loess trend component
    trend = "trend (loess)"
    ts = ts(dt[, value], frequency=freq)
    stl(ts, "periodic")[[1]][, "trend"]
  } else {
    # Linear trend
    trend = "trend (linear)"
    predict(lm(value~date, dt))
  }

  dt[, trend := tr]
  yrange = if(missing(yrange)) dt[, range(value, na.rm=T)] else yrange
  subtitle = if(missing(subtitle)) sprintf('%s (%s / %s)',
    dt[, paste(range(year(date)), collapse="-")], unit,
    switch(as.character(freq), `1`="year", `2`="season", `12`="month")) else subtitle

  # Period bands
  xBands = lapply(dt[, seq(min(date), max(date), by="year")], function(x) list(
    from = datetime_to_timestamp(x),
    to = datetime_to_timestamp(ceiling_date(x, "year")),
    color = c("transparent", alpha(pal[["light"]], .4))[1+odd(year(x))]
  ))

  # Flags
  flag = dt[dt[, c(which.min(value), which.max(value))]
  ][, `:=`(
    label = c("Min.", "Max."),
    unit = unit
  )]

  highchart(type="stock") %>%
    hc_add_series(dt, type="area", hcaes(x=date, y=value),
      name=sprintf('%s (%s)', name, unit),
      fillColor=list(linearGradient=c(0, 0, 0, 240),
        stops=list(c(0, alpha(pal[[2]], .5)), c(1, "transparent"))),
      color=pal[[2]], lineWidth=switch(as.character(freq), `1`=3, 2)) %>%
    hc_add_series(dt, type="line", hcaes(x=date, y=ltn),
      name=sprintf('LTN (%s)', unit),
      color=pal[[1]], lineWidth=switch(as.character(freq), `1`=3, 2),
      marker=list(enabled=FALSE)) %>%
    hc_add_series(dt, type="line", hcaes(x=date, y=trend),
      name=trend,
      color=pal[["orange"]], lineWidth=3, marker=list(enabled=FALSE)) %>%

    hc_add_series(flag, type="flags",
      hcaes(x=date, y=value, title=sprintf('%s %s (%s %s)', label, year, comma(value), unit)),
      color=pal[["black"]], fillColor=pal[["light"]],
      shape="squarepin", showInLegend=FALSE) %>%

    hc_legend(enabled=TRUE, align="right") %>%
    hc_tooltip(valueSuffix=paste("", unit)) %>%
    hc_xAxis(type="datetime", dateTimeLabelFormats=list(month="%Y %b"), plotBands=xBands) %>%
    hc_yAxis(min=yrange[1], max=yrange[2] + 0.05 * diff(yrange)) %>%
    hc_rangeSelector(enabled=(freq>1), inputEnabled=FALSE) %>%
    hc_navigator(enabled=FALSE) %>%
    hc_scrollbar(enabled=FALSE) %>%
    hc_chart(zoomType="x") %>%
    hc_themed(subtitle=subtitle, ...)
}


#' Plot time-series intra-annual variability (highcharts)
#'
#' @param data data.table to plot with columns `date` and `value` (will use `date_start`
#'   if `date` is missing)
#' @param unit display unit
#' @param polar use polar (circular) axis
#' @param unit.threshold maximum threshold to convert from km³ to MCM unit (default: 10 km³)
#' @inheritParams hc_themed
#' @inheritDotParams hc_themed
#'
#' @import data.table
#' @import highcharter
#'
#' @return highcharter object
#'
#' @examples
#' dt <- DATA[iso3=="ken" & id=="rainfall" & period=="month"]
#' plot_profile(dt, unit="km³")
#' plot_profile(dt, unit="km³", polar=TRUE)
#'
#' @export
plot_profile <- function(data, unit=NA,
  polar=FALSE, unit.threshold=10, subtitle=NULL, ...) {

  dt = copy(data)
  if(!"date" %in% names(dt)) setnames(dt, "date_start", "date")
  setorder(dt, date)

  if(dt[, .(unit)=="km³" & (max(value, na.rm=T) < unit.threshold)]) {
    dt[, value := 1000 * value]
    unit = "MCM"
  }

  subtitle = if(missing(subtitle)) sprintf('%s (%s / month)',
    dt[, paste(range(year(date)), collapse="-")], unit) else subtitle

  # Label years
  ymax = dt[, max(year(date))]

  dt = dt[, .(
    date = max(date),
    value = mean(value, na.rm=T),
    sd = sd(value, na.rm=T),
    # Highlight past 3 years
    value_1 = mean(fifelse(year(date)==(ymax-0), value, NA_real_), na.rm=T),
    value_2 = mean(fifelse(year(date)==(ymax-1), value, NA_real_), na.rm=T),
    value_3 = mean(fifelse(year(date)==(ymax-2), value, NA_real_), na.rm=T)
  ), by=.(month(date))][, `:=`(
    ymin = value-sd,
    ymax = value+sd
  )]

  # Monthly bands
  xBands = lapply(dt[, seq.Date(min(date), max(date), by="month")],
    function(x) list(
      from = datetime_to_timestamp(x),
      to = datetime_to_timestamp(ceiling_date(x, unit="month")-1),
      color = c("transparent", alpha(pal[["light"]], .4))[1+odd(month(x))]
    ))

  highchart() %>%
    hc_chart(zoomType="x", polar=polar) %>%
    hc_add_series(dt, type="arearange",
      hcaes(x=date, low=ymin, high=ymax), name="+/- Std. Dev.",
      color=pal[[2]], marker=list(enabled=FALSE)) %>%

    hc_add_series(dt, type=c("column", "line")[polar+1],
      hcaes(x=date, y=value_3), name=ymax-2,
      color=alpha(pal[["orange"]], .3)) %>%
    hc_add_series(dt, type=c("column", "line")[polar+1],
      hcaes(x=date, y=value_2), name=ymax-1,
      color=alpha(pal[["orange"]], .6)) %>%
    hc_add_series(dt, type=c("column", "line")[polar+1],
      hcaes(x=date, y=value_1), name=ymax-0,
      color=alpha(pal[["orange"]], .9)) %>%

    hc_add_series(dt, type="line", hcaes(x=date, y=value),
      name=sprintf('LTN (%s)', unit),
      color=pal[[2]], lineWidth=3, marker=list(enabled=TRUE)) %>%

    hc_tooltip(valueSuffix=paste("", unit)) %>%
    hc_legend(enabled=TRUE, align="right") %>%
    hc_xAxis(type="datetime", dateTimeLabelFormats=list(month="%b"),
      plotBands=xBands, labels=list(step=1),
      lineWidth=1-polar) %>%
    hc_yAxis(opposite=TRUE, showFirstLabel=!polar) %>%
    hc_themed(subtitle=subtitle, ...)
}

#' Plot simple variable time-series (highcharts)
#'
#' @param data data.table to plot with columns `date` and `value` (will use `date_end`
#'   if `date` is missing)
#' @param unit display unit
#' @param color an optional built-in color name (default "navy")
#'
#' @return
#'
#' @examples
#' plot_ts(DATA[iso3=="mli" & sheet=="sheet1" & id=="rainfall"])
#'
#' @export
plot_tss <- function(data, unit=NA, color=names(pal)) {

  if(!"date" %in% names(dt)) setnames(dt, "date_end", "date", skip_absent=TRUE)

  highchart() %>%
    hc_chart(zoomType="x") %>%
    hc_add_series(dt, type="area",
      hcaes(x=date, y=value), name=dt[1, id],
      color=color, fillColor=alpha(color, .2), marker=list(enabled=TRUE)) %>%

    hc_xAxis(type="datetime", dateTimeLabelFormats=list(month="%e %b")) %>%
    hc_tooltip(valueSuffix=paste("", unit),
      pointFormat="{series.name}: <strong>{point.y:,.2f}</strong><br/>") %>%
    hc_legend(enabled=TRUE, align="top") %>%
    hc_themed()
}

