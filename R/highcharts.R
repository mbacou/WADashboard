#' Plot yearly ECC time-series (highcharts)
#'
#' @param data output of [wc_ecc]
#' @inheritParams hc_themed
#' @import highcharter data.table
#'
#' @examples
#' data <- data.table(
#'   year = rep(2001:2020, each=5),
#'   ecc = sample(1:100/100, 5*20, replace=T),
#'   ecc_dr = sample(1:100/100, 5*20, replace=T),
#'   ecc_xs = sample(1:100/100, 5*20, replace=T),
#'   ecc_tp = sample(1:100/100, 5*20, replace=T),
#'   weight = sample(1:100/100, 5*20, replace=T)
#'   )
#' plot_ecc(data)
#'
#' @export
plot_ecc <- function(
  data,
  title =  "Historical Claim Costs across Perils",
  subtitle = "As percent of limit, 2001-2019 average ECC",
  ...) {

  data[, year := as.character(year)]

  highchart() %>%
    hc_chart(zoomType="y") %>%
    hc_add_series(data=data[, .(ecc=weighted.mean(ecc_dr, weight)), by=year],
      "column", hcaes(x=year, y=100*ecc), name="Drought", color=pal[["yellow"]]) %>%
    hc_add_series(data=data[, .(ecc=weighted.mean(ecc_xs, weight)), by=year],
      "column", hcaes(x=year, y=100*ecc), name="Excess Rain", color=pal[["blue"]]) %>%
    hc_add_series(data=data[, .(ecc=weighted.mean(ecc_tp, weight)), by=year],
      "column", hcaes(x=year, y=100*ecc), name="Frost", color=pal[["aqua"]]) %>%
    hc_add_series(data=data[, .(ecc=weighted.mean(ecc, weight)), by=year],
      "line", hcaes(x=year, y=100*ecc), name="Total", color=pal[["red"]], lineWidth=2,
      marker=list(enabled=T, radius=3)) %>%
    hc_xAxis(type="category") %>%
    hc_yAxis(min=0) %>%
    hc_themed(title, subtitle, ...) %>%
    hc_exporting(enabled=TRUE)
}

#' Boxplot yearly ECC time-series (highcharts)
#'
#' @param data output of [wc_ecc]
#' @param prob which percentile threshold(s) to draw on charts
#' @inheritParams hc_themed
#' @import highcharter data.table
#'
#' @examples
#' data <- data.table(
#'   year = rep(2001:2020, each=5),
#'   ecc = sample(1:100/100, 5*20, replace=T)
#'   )
#' boxplot_ecc(data)
#'
#' @export
boxplot_ecc <- function(
  data,
  prob = .95,
  title =  "Historical Claim Costs across Years",
  subtitle = "As percent of limit, 2001-2019 average ECC with VaR line in red",
  ...) {

  intcp = data[, .(
    prob = prob,
    ecc = quantile(ecc, prob, na.rm=T)
  )]

  data[, year := as.character(year)]
  dt = data_to_boxplot(data, 100*ecc, year, name="ECC", add_outliers=T)

  highchart() %>%
    hc_chart(zoomType="y") %>%
    hc_add_series_list(dt) %>%
    hc_xAxis(type="category") %>%
    hc_yAxis(min=0, max=intcp[1, 100*ecc+1],
      labels=list(format="{value}%"), plotLines=list(
        list(value=intcp[1, 100*ecc], color=pal[["red"]], width=1.2, dashStyle="dash"))
    ) %>%
    hc_themed(title, subtitle, ...) %>%
    hc_exporting(enabled=TRUE)
}

#' Plot yearly ECC density (highcharts)
#' @param data output of [wc_ecc]
#' @param prob which percentile threshold(s) to draw
#' @inheritParams hc_themed
#' @import highcharter data.table
#' @export
plot_ecc_density <- function(
  data,
  prob = .95,
  title = "Probability Density of Claim Costs",
  subtitle = "As percent of limit, 2001-2019 average ECC with %s VaR",
  ...) {

  subtitle = sprintf(subtitle, pct(prob))

  dt = data[, .(ecc = 100*ecc)]
  intcp = dt[, .(
    prob = prob,
    ecc = quantile(ecdf(ecc), prob, na.rm=T)
  )]

  hchart(density(dt[, ecc]), type="area", name="density") %>%
    hc_xAxis(min=0, max=dt[, max(ecc)], labels=list(format="{value}"), plotLines=list(
      list(value=intcp[, min(ecc)], color=pal[["red"]], width=1.2, dashStyle="dash")
    )) %>%
    hc_legend(enabled=FALSE) %>%
    hc_themed(title, subtitle, ...)
}

#' Plot dekadal index time series (highcharts)
#' @param data biophysical time-series (output of `wc_extract()`)
#' @param type index type `tp` (precipitation) or `temp` (temperature)
#' @param sos start of season threshold (default: 35 mm or 8°C Tmin for blooming)
#' @param yrange y-axis min and max
#' @inheritParams hc_themed
#' @import highcharter data.table
#' @importFrom lubridate ceiling_date year<-
#' @examples
#' dt <- readRDS(file.path(getOption("wc.shared"), "tmp", "2020-burn_lockton_bio.rds"))
#' plot_clim(dt[code=="bom_rain"])
#'
#' @export
plot_clim <- function(
  data,
  type = c("precipitation", "tmin", "tmax"),
  sos = switch(type, precipitation=35, 2),
  yrange = NULL,
  title = "Climate Normals by Dekad",
  subtitle = "Average across portfolio locations, 1990-2020 (mm)",
  ...) {

  type = match.arg(type)
  unit = c(" °C", " mm")[1 + (type=="precipitation")]

  dt = data[, date := as.Date(date)][, .(
    date = min(date),
    value = fcase(
      type=="tmin", min(value, na.rm=T),
      type=="tmax", max(value, na.rm=T),
      default = sum(value, na.rm=T))
  ), keyby=.(loc_id, year=year(date), dekad=dekad(date, type="year"))]

  year(dt$date) = year(Sys.Date())
  dt = dt[, .(
    value = mean(value, na.rm=T),
    sd = sd(value, na.rm=T),
    # Past 3 years
    value_1 = mean(fifelse(year==max(year)-0, value, NA_real_), na.rm=T),
    value_2 = mean(fifelse(year==max(year)-1, value, NA_real_), na.rm=T),
    value_3 = mean(fifelse(year==max(year)-2, value, NA_real_), na.rm=T)
  ), by=.(date)][, `:=`(
    ymin = value-sd,
    ymax = value+sd
  )]

  # Label years
  ymax = data[, max(year(date))]
  yrange = if(missing(yrange)) dt[, c(min(value, na.rm=T), max(value_1, na.rm=T))] else yrange

  # Monthly bands
  xBands = lapply(dt[, seq(min(date), max(date), by="month")], function(x) list(
    from = datetime_to_timestamp(as.Date(x)),
    to = datetime_to_timestamp(ceiling_date(as.Date(x), unit="month")-1),
    color = c("transparent", alpha("#e9e9e9", .4))[1+odd(month(x))]
  ))

  # SOS vertical intercepts
  intcp = dt[, sos := value >= sos][(sos) & !shift(sos), date]
  xLines = lapply(intcp, function(x) list(
    value=datetime_to_timestamp(x), color=pal[["red"]], width=1.2,
    label=list(text=x, align="left", color=pal[["red"]])))

  yLines = list(
    list(value=sos, color=pal[["red"]], width=1.2,
      label=list(text=sprintf("%s%s", sos, unit), align="left")),
    list(value=0, color=pal[["black"]], width=1.2))

  p = highchart() %>%
    hc_chart(zoomType="x") %>%
    hc_add_series(dt, type="arearange",
      hcaes(x=date, low=ymin, high=ymax), name="+/- Std. Dev.",
      color=pal[[1]], fillOpacity=.1, marker=list(enabled=FALSE)) %>%

    hc_add_series(dt, type="column", hcaes(x=date, y=value_3), name=ymax-2,
      color=alpha(pal[[11]], .3)) %>%
    hc_add_series(dt, type="column", hcaes(x=date, y=value_2), name=ymax-1,
      color=alpha(pal[[11]], .6)) %>%
    hc_add_series(dt, type="column", hcaes(x=date, y=value_1), name=ymax-0,
      color=alpha(pal[[11]], .9)) %>%

    hc_add_series(dt, type="line", hcaes(x=date, y=value), name="LTN",
      color=pal[[1]], lineWidth=2, marker=list(enabled=TRUE)) %>%

    hc_tooltip(valueSuffix=unit) %>%
    hc_legend(enabled=TRUE, align="right") %>%
    hc_xAxis(type="datetime", dateTimeLabelFormats=list(month="%e %b"),
      plotBands=xBands, plotLines=xLines) %>%
    hc_yAxis(min=yrange[1], max=yrange[2], plotLines=yLines) %>%
    hc_themed(title, subtitle, ...)

  return(p)
}

#' Plot dekadal index time series with trends (highcharts)
#' @param data biophysical time-series (output of [wc_extract])
#' @param type precipitation or temperature variable
#' @param yrange y-axis min and max
#' @inheritParams hc_themed
#' @import highcharter data.table
#' @importFrom stats ts stl
#' @importFrom gtools odd even
#' @examples
#' dt <- readRDS(file.path(getOption("wc.shared"), "tmp", "2020-burn_lockton_bio.rds"))
#' plot_ts(dt[code=="bom_rain"])
#'
#' @export
hc_ts <- function(
  data,
  type = c("precipitation", "temperature"),
  yrange = NULL,
  title = "Long-Term Cumulative Rainfall by Dekad",
  subtitle = "Average across portfolio locations, 1990-2020 (mm)",
  ...) {

  type = match.arg(type)
  unit = c("°C", "mm")[1+(type=="precipitation")]

  dt = data[, .(
    date = min(date),
    value = fifelse(type=="temperature", min(value, na.rm=T), sum(value, na.rm=T))
  ), keyby=.(loc_id, year=year(date), dekad=dekad(date, type="year"))
  ][, ltn := cumsum(value)/1:.N, by=.(loc_id, dekad)
  ][, .(
    value = mean(value, na.rm=T),
    ymin = min(value, na.rm=T),
    ymax = max(value, na.rm=T),
    ltn = mean(ltn, na.rm=T)
  ), keyby=.(date)]

  # Add trend decomposition
  ts = ts(dt[, value], start=c(dt[, min(year(date))], 1), frequency=36)
  tr = stl(ts, "periodic")
  dt[, trend := tr[[1]][, "trend"]]

  yrange = if(missing(yrange)) dt[, c(min(value, na.rm=T), max(value, na.rm=T))] else yrange

  xBands = lapply(dt[, seq(min(date), max(date), by="year")], function(x) list(
    from = datetime_to_timestamp(as.Date(x)),
    to = datetime_to_timestamp(as.Date(x)+365-1),
    color = c("transparent", alpha("#e9e9e9", .4))[1+odd(year(x))]
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
    hc_chart(zoomType="x") %>%
    hc_themed(title, subtitle, ...)

  return(p)
}

#' Plot product recipes (highcharts)
#' @param args pricing arguments
#' @param x (optional) which location ID (loc_id) to draw
#' @inheritParams hc_themed
#' @import highcharter data.table
#' @examples
#' pts <- data.table(loc_id=1, X=1, Y=1, day="2020-03-02")
#' args <- wc_args(pts, crop="maiz", pay_grm=.3)
#' plot_rcp(args)
#'
#' @export
plot_rcp <- function(
  args,
  x = NULL,
  title = "Coverage Periods and Payout Factors | %s",
  subtitle = "Range of payout factors by index type (% of S.I.)",
  ...) {

  x = if(missing(x)) args$prd[, first(loc_id)] else x
  title = sprintf(title, args$product)

  # Plot coverage periods
  dt = args$prd[loc_id==x
  ][args$trg_pct, on=.(period), mult="all"
  ][, `:=`(
    period = factor(period, levels=paste0("prd_", 0:8), labels=names(wc_period)),
    type = factor(type, levels=INDEX, labels=toupper(INDEX))
  )][pay_lo > 0
  ][is.na(pay_hi), `:=`(
    pay_hi = pay_lo,
    pct_hi = pct_lo
  )][, `:=`(
    pay_lo = 100* pay_lo,
    pay_hi = 100* pay_hi,
    pct_lo = 100* pct_lo,
    pct_hi = 100* pct_hi
  )]

  # Fix before we switch to `wcengine`
  dt.long = dt[, data.table(
    type = rep(type, 2),
    date = c(start, end),
    pay = c(pay_lo, pay_hi)
  )][, col := idx_cols[tolower(type)]
  ][order(type, date, pay)]

  # Period annotations
  planted = dt[1, datetime_to_timestamp(c(day, day-5))]
  dt.prd = unique(dt, by="start")[, data.table(
    period = rep(period, 2),
    date = c(start, end)
  )][order(period, date)][, jitter := -8* (rep(1:(.N/2), each=2))]

  bands = apply(dt.prd, 1, function(x) list(
    value = datetime_to_timestamp(as.Date(x[["date"]])),
    color = pal[["gray-lte"]],
    dashStyle="dot", width=2
  ))
  bands[[length(bands)+1]] = list(
    value = planted[1],
    color = pal[["red"]],
    dashStyle="dash", width=2,
    label = list(text="anchor date", align="left", color=pal[["red"]], x=-10)
  )

  labels = apply(dt.prd[, .(date=mean(date), jitter=min(jitter)), by=period], 1,
    function(x) list(
      borderWidth=0, shape="none", padding=3, backgroundColor=pal[["gray-lte"]],
      point=list(x=datetime_to_timestamp(as.Date(x[["date"]])), y=x[["jitter"]], xAxis=0, yAxis=0),
      text=x[["period"]], y=8)
  )

  p =  highchart() %>%
    hc_add_series(dt.prd, type="line", hcaes(date, jitter, group=period),
      lineWidth=2, connectNulls=FALSE, color=pal[["gray-lte"]],
      marker=list(symbol="triangle-down", radius=3), showInLegend=FALSE) %>%
    hc_add_series(dt.long, type="line", hcaes(date, pay, group=type),
      color=dt.long[, unique(col)], lineWidth=4, connectNulls=FALSE,
      dataLabels=list(enabled=TRUE, format="{point.y}%", style=list(color="#777")),
      marker=list(enabled=TRUE, symbol="circle", radius=3)) %>%
    hc_xAxis(type="datetime", min=planted[2], plotLines=bands, minTickInterval=10) %>%
    hc_yAxis(max=dt.long[, max(pay)], labels=list(format="{value}%"), showFirstLabel=FALSE) %>%
    hc_annotations(list(labels=labels)) %>%
    hc_tooltip(enabled=TRUE, format="{point.x}<br/>{point.y}%") %>%
    hc_themed(title, subtitle, ...)

  return(p)
}


#' Plot yearly index time series with EMA trends (highcharts)
#' @param data output of [wc_trg_*], see examples
#' @inheritParams highcharter::hw_grid
#' @inheritParams hc_themed
#' @inheritDotParams hc_themed
#' @import data.table highcharter
#' @importFrom scales alpha
#' @importFrom stats ts
#' @importFrom TTR EMA
#' @importFrom scales rescale
#' @examples
#' dt <- data.table(
#'  loc_id = 1:3,
#'  period = rep(c("prd_1", "prd_5"), 3),
#'  year = rep(1990:2020, each=3*2),
#'  idx = rnorm(31*3*2, 10),
#'  idx_lo = 8,
#'  idx_hi = 12,
#'  ecc = rnorm(31*3*2, .5)
#' )
#' plot_idx(dt)
#'
#' @export
plot_idx <- function(
  data,
  title = "Index Trend",
  subtitle = "Average across portfolio locations with EMA trend, 1990-2020 (mm)",
  label = NULL,
  rowheight = 280,
  ncol = 1,
  ...) {

  dt = data[, .(
    idx = mean(idx, na.rm=T),
    sd = sd(idx, na.rm=T),
    idx_lo = round(mean(idx_lo, na.rm=T)),
    idx_hi = round(mean(idx_hi, na.rm=T)),
    ecc = mean(ecc, na.rm=T)
  ), by=.(year, period)
  ][, ecc := fifelse(is.na(ecc) | ecc==0, 0, rescale(ecc, to=c(.3, 1)))
  ][, col := alpha(alpha("#e9e9e9", .6), ecc)]

  dt = split(dt, by="period")
  l = lapply(dt, function(x) {

    ymin = x[, min(pmin(idx-sd, idx_lo, idx_hi, na.rm=T), na.rm=T)]
    ymax = x[, max(pmax(idx+sd, idx_lo, idx_hi, na.rm=T), na.rm=T)]
    col = wc_period_cols[x[1, period]]
    col = if(is.na(col)) pal[["green"]] else col

    # Bands
    xBands = lapply(1:nrow(x), function(i) list(
      from=x[i, year-.5], to=x[i, year+.5], color=x[i, col]))

    # Add trend
    tr = try(EMA(ts(x[, idx]), 5))
    tr = if(class(tr)=="try-error") EMA(ts(x[year<max(year), idx]), 5) else tr
    x[1:length(tr), trend := tr]

    highchart() %>%
      hc_add_series(data=x, "arearange", name="+/- Std.",
        hcaes(x=year, low=idx-sd, high=idx+sd),
        color=col, marker=list(enabled=FALSE)) %>%
      hc_add_series(data=x, "line", name="Trend", lineWidth=1.4,
        hcaes(x=year, y=trend), color=pal[["black"]],
        marker=list(enabled=NA, symbol="circle", radius=1.2)) %>%
      hc_add_series(data=x, "line", name=paste("Index -", x[1, period]),
        hcaes(x=year, y=idx), lineWidth=3, color=col,
        marker=list(enabled=TRUE, symbol="circle")) %>%

      hc_yAxis(softMin=ymin, softMax=ymax, plotLines=list(
        list(color=pal[["red"]], value=x[1, idx_hi], width=1.2,
          label=list(text="severe")),
        list(color=pal[["red"]], value=x[1, idx_lo], width=1.2, dashStyle="dot",
          label=list(text="medium"))
      )) %>%
      hc_xAxis(plotBands=xBands) %>%
      hc_themed(
        title[x[1, period]==data[1, period]],
        subtitle[x[1, period]==data[1, period]],
        x=x[, max(year)-4], y=ymax+ymin, ...) %>%
      hc_exporting(enabled=TRUE)
  })

  return(
    hw_grid(l, ncol=ncol, rowheight=rowheight, add_htmlgrid_css=FALSE)
  )
}

#' Plot index density (highcharts)
#' @param data output of `wc_trg_*`
#' @inheritParams highcharter::hw_grid
#' @inheritParams hc_themed
#' @inheritDotParams hc_themed
#' @import data.table highcharter
#' @examples
#' dt <- data.table(
#'  loc_id = 1:3,
#'  period = rep(c("prd_1", "prd_5"), 3),
#'  year = rep(1990:2020, each=3*2),
#'  idx = rnorm(31*3*2, 10),
#'  idx_lo = 8,
#'  idx_hi = 12
#' )
#' plot_idx_density(dt)
#'
#' @export
plot_idx_density <- function(
  data,
  title = "Index Density Distribution",
  subtitle = "Across portfolio locations, 1990-2020 (mm)",
  rowheight = 280,
  ncol = 1,
  ...) {

  dt = split(data, by="period")
  l = lapply(dt, function(x) {

    p = x[1, period]
    fmt = switch(p,
      prd_0 = "Lognormal - mean: %.2f | sd: %.2f",
      prd_1 = "Gamma - mu: %.2f | sigma: %.2f",
      prd_2 = "Gamma - mu: %.2f | sigma: %.2f",
      prd_3 = "ECDF - mean: %.2f | sd: %.2f",
      prd_2 = "ECDF - mean: %.2f | sd: %.2f",
      prd_5 = "Gamma - mu: %.2f | sigma: %.2f",
      prd_6 = "ECDF - mean: %.2f | sd: %.2f",
      prd_7 = "Lognormal - mean: %.2f | sd: %.2f",
      prd_8 = "Gamma - mu: %.2f | sigma: %.2f",
      "")

    coeff = if("shape" %in% names(x))
      x[, .(shape=mean(shape, na.rm=T), rate=mean(rate, na.rm=T))] else
        x[, .(shape=mean(idx, na.rm=T), rate=sd(idx, na.rm=T))]
    fit = data.table(period=p, x=0:x[, max(idx, na.rm=T)]
    )[, y := fcase(
      period %in% c("prd_0", "prd_7"), dlnorm(x, coeff$shape, coeff$rate),
      period %in% c("prd_1", "prd_2", "prd_5", "prd_8"), dgamma(x, coeff$shape, coeff$rate),
      default = NA_real_
    )]

    highchart() %>%
      hc_add_series(density(x[, idx]), type="area", name=p, color=wc_period_cols[[p]]) %>%
      hc_add_series(fit, type="line",
        hcaes(x=x, y=y), color=wc_period_cols[[p]], name="fitted")  %>%
      hc_xAxis(plotLines = list(
        list(color=pal[["red"]], value=x[, mean(idx_hi)], width=1.2,
          label=list(text="severe", align="left")),
        list(color=pal[["red"]], value=x[, mean(idx_lo)], width=1.2,
          label=list(text="medium", align="left"), dashStyle="dot")
      )) %>%
      hc_credits(enabled=TRUE, position=list(align="right"),
        text=sprintf(fmt, coeff$shape, coeff$rate)) %>%
      hc_themed(title[p==data[1, period]], subtitle[p==data[1, period]], ...)
  })

  return(
    hw_grid(l, ncol=ncol, rowheight=rowheight, add_htmlgrid_css=FALSE)
  )
}


#' Plot correlation matrix (highcharts)
#'
#' @param data output of [wc_ecc]
#' @inheritParams hc_themed
#' @inheritDotParams hc_themed
#' @importFrom scales rescale
#' @import data.table highcharter
#' @examples
#' data <- data.table(
#'   ecc_grm = rnorm(20),
#'   ecc_cdd = rnorm(20),
#'   ecc_crf = rnorm(20),
#'   ecc_crd = rnorm(20),
#'   ecc_crw = rnorm(20)
#' )
#' plot_cor(data)
#'
#' @export
plot_cor <- function(
  data,
  title = NA,
  subtitle = NA,
  ...) {

  cols = pal[c("red", "yellow", "white", "aqua", "blue")]

  dt = 100 * cor(data[, .(
    grm = ecc_grm > 0,
    cdd = ecc_cdd > 0,
    crf = ecc_crf > 0,
    crd = ecc_crd > 0,
    crw = ecc_crw > 0,
    erd = ecc_erd > 0,
    fdd = ecc_fdd > 0
  )])
  dimnames(dt) = lapply(dimnames(dt), toupper)
  dt = as.data.table(dt, keep.rownames=TRUE)
  dt = melt(dt, id.vars="rn", variable.name="y", variable.factor=FALSE)

  p = hchart(dt, type="heatmap", hcaes(x=rn, y=y, value=value), name="Correlation.",
    marker=list(enabled=TRUE), dataLabels=list(enabled=TRUE)) %>%
    hc_colorAxis(stops=color_stops(20, cols), min=-100, max=100) %>%
    hc_legend(enabled=TRUE, verticalAlign="top", layout="horizontal", align="right") %>%
    hc_xAxis(title=NULL) %>% hc_yAxis(title=NULL) %>%
    hc_themed(title=title, subtitle=subtitle, ...) %>%
    hc_exporting(enabled=TRUE)

  return(p)
}


#' Plot payout curves (highcharts)
#' @param data output of [wc_price] at 1 trigger location (single contract)
#' @param args pricing arguments
#' @inheritParams highcharter::hw_grid
#' @inheritParams hc_themed
#' @inheritDotParams hc_themed
#' @import data.table highcharter
#' @examples
#' pts <- data.table(loc_id=1, X=30, Y=-17, day="2020-12-01")
#' args <- wc_args(pts, product="MAIZ-ZWE-05-CB01", code="rfe")
#' data <- wc_price(args=args)
#' plot_tick(data, args)
#'
#' @export
plot_tick <- function(
  data,
  title = NA,
  subtitle = NA,
  rowheight = 200,
  ...) {

  prd = data[, unique(period)]
  p = lapply(seq_along(prd), function(x) {

    p = prd[x]
    dt = data[period == p]
    t = dt[1, type]
    dt <- rbind(fill=TRUE, dt, data.table(
      value = c(dt[, min(value)-c(1:5)], dt[, max(value)+c(1:5)]),
      pay = dt[, rep(sort(c(0, max(pay)), decreasing=t %in% c("grm", "crf")), each=5)]
    ))
    setorder(dt, value, pay)

    highchart(height=rowheight) %>%
      hc_chart(zoomType="x") %>%
      hc_add_series(dt, type="line", hcaes(x=value, y=100*pay), color=idx_cols[[t]],
        name=sprintf("%s (%s)", toupper(t), p), width=1,
        marker=list(enabled=TRUE, radius=3),
        tooltip=list(pointFormat="Payout: <strong>{point.y:,.0f}%</strong>"))  %>%
      hc_xAxis(min=dt[, pmax(min(value), 0)], max=dt[, max(value)+1]) %>%
      hc_yAxis(title=list(text="Payout (% of S.I.)"), labels=list(format="{value}%")) %>%
      hc_legend(align="left") %>%
      hc_themed(title=title[x==1], subtitle=subtitle[x==1], ...) %>%
      hc_exporting(enabled=TRUE)
  })

  return(p)
}


#' Plot temperature indices (highcharts)
#'
#' @param data output of [wc_trg*]
#' @inheritParams hc_themed
#' @inheritDotParams hc_themed
#' @importFrom scales rescale
#' @import data.table highcharter
#' @examples
#' data <- data.table(
#'   year = rep(2000:2010, each=10),
#'   loc_id = 1:10,
#'   idx = rnorm(10, mean=80)
#' )
#' plot_idx_mosaic(data)
#'
#' @export
plot_idx_mosaic <- function(
  data,
  title = NA,
  subtitle = NA,
  ...) {

  cols = c("transparent", pal[c("gray-lte", "yellow", "orange", "red", "purple")])

  p = hchart(data, type="heatmap", hcaes(x=year, y=factor(loc_id), value=idx), name="Index",
    marker=list(enabled=TRUE, lineWidth=0)) %>%
    hc_colorAxis(stops=color_stops(20, cols), min=0) %>%
    hc_legend(enabled=TRUE, title="Index", verticalAlign="top", layout="horizontal", align="left") %>%
    hc_xAxis(title=NULL, majorTickInterval=1) %>%
    hc_themed(title=title, subtitle=subtitle, ...) %>%
    hc_exporting(enabled=TRUE)

  return(p)
}
