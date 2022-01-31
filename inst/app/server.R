#####################################################################################
# Title:   IWMI WA+ Dashboard (SERVER)
# Date:    October 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

function(input, output, session) {

  session$allowReconnect(TRUE)

  s = reactiveValues(
    iso3 = init$iso3,
    date = init$date,
    var = list(var="var_incremental_etnat", color="green")
  )

  layers = reactive(
    c(input$chkLayer_1, input$chkLayer_2, input$chkLayer_3)
  )

  dt = reactive(
    data[iso3==s$iso3]
  )

  dtf = reactive(
    dt()[date_end==s$date]
  )

  # Sheet 1 ----
  output$d3_sheet1 = renderD3({
    r2d3(dtf()[sheet=="sheet1"], script="./www/js/sheet_1.js")
  })

  output$d3_sheet2 = renderD3({
    r2d3(dtf()[sheet=="sheet2"], script="./www/js/sheet_2.js")
  })

  output$d3_sheet3 = renderD3({
    r2d3(dtf()[sheet=="sheet3"], script="./www/js/sheet_3.js")
  })

  output$tb_basin = renderTable(
    hover=T, spacing="xs", colnames=F, align="lr", width="100%",
    melt(as.data.table(ISO3[[s$iso3]])[, `:=`(
      `authorities` = sprintf(
        '%s <a href="%s"><i class="fa fa-external-link fa-fw"></i></a>', authorities, url),
      `area` = sprintf("%s ha", comma(area)),
      `population` = sprintf("%s", comma(`population`)),
      `annual rainfall` = sprintf("%s mm", comma(`annual rainfall`)),
      `annual ET` = sprintf("%s mm", comma(`annual ET`)),
      `irrigated area` = sprintf("%s ha", comma(`irrigated area`)),
      `hydro power` = sprintf("%s GWh/year", comma(`hydro power`))
    )], id.vars=1)[!variable %in% c("admin", "water", "source", "url", "unit"), .(
      variable = sprintf('<span class="text-info">%s</span>', str_to_title(variable)),
      value
    )]
  )

  output$ui_score_prod = renderUI({
    dt = fread("
    variable, value, status, max
    Agr. Water Productivity, 12, warning, 60
    Utilizable Flow, 79, success, 120
    Blue water availability, 30, danger, 110
      ")
    lapply(1:nrow(dt), function(x) dt[x,
      progressBar(paste0("pbg-", x), value, total=max,
        title=span(class="pt-3", variable), status=status, display_pct=T)]
    ) %>% tagList()
  })

  output$ui_score_sust = renderUI({
    dt = fread("
    variable, value, status, max
    Green water availability, 12, warning, 60
    Sustaining Rainfall, 79, success, 120
    Storage Change, 20, danger, 100
      ")
    lapply(1:nrow(dt), function(x) dt[x,
      progressBar(paste0("pbg-", x), value, total=max,
        title=span(class="pt-3", variable), status=status, display_pct=T)]
    ) %>% tagList()
  })

  observeEvent(input$txtISO3, {
    s$iso3 = tolower(input$txtISO3)
  })

  observeEvent(input$txtDate, {
    # Always last day of selected month
    s$date = ceiling_date(ym(input$txtDate), "months") - days(1)
  })

  observeEvent(input$btnScore, {
    updateNavbarPage(session, "navPage", selected="Scorecard")
  })

  observeEvent(input$btnRefresh, {
    updateNavbarPage(session, "navPage", selected="About WA+")
  })

  # Map
  output$map = renderLeaflet(map_init(init$iso3))

  # Filters ----
  observeEvent(s$iso3, {
    # Update map
    leafletProxy("map") %>% map_update(s$iso3)

    # Update time slider
    dt <- dt()[sheet=="sheet1"]
    updateSliderTextInput(session, "txtDate", NULL,
      dt[order(date_end), format(unique(date_end), "%Y %b")],
      selected=dt[, format(max(date_end), "%Y %b")])
  })

  # Toggle map layers ----
  observeEvent(layers(), {
    leafletProxy("map") %>% map_toggle(layers())
  })

  output$uiLegend = renderUI(
    if(length(layers())>0) lapply(layers(), function(x)
      tagList(h5(class="text-info", x), img(class="img-responsive",
        src=sprintf(LAYERS[["FAO"]]$legend, LAYERS[["FAO"]]$layers[[x]]))
      )) else p(class="mt-2 text-muted", "No layer selected.")
  )

  output$uiInfo = renderUI(
    if(length(layers())>0) lapply(layers(), function(x)
      tagList(h5(class="text-info", x), p("")
      )) else p(class="mt-2 text-muted", "No layer selected.")
  )

  # Plots ----
  output$plot_ts = renderHighchart({
    req(s$var$var)
    plot_ts(dt()[id==s$var$var], s$var$color)
  })

  observeEvent(input$bar_clicked, {
    e = input$bar_clicked
    updateTextAreaInput(inputId="objSelected",
      value=paste(s$date, e$var, ": ", comma(as.numeric(e$value), accuracy=0.01)))
    s$var = e
  })

}
