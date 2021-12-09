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
    year = year(init$date),
    var = list(var="var_incremental_etnat", color="green")
  )

  layers = reactive(
    c(input$chkLayer_1, input$chkLayer_2, input$chkLayer_3)
  )

  dt = reactive(
    data[iso3==s$iso3]
  )

  # Map
  output$map = renderLeaflet(map_init(init$iso3))

  # Sheet 1
  output$d3_sheet1 = renderD3({
    r2d3(dt()[sheet=="sheet1" & year==s$year], script="./www/js/sheet_1.js")
  })

  output$d3_sheet2 = renderD3({
    r2d3(dt()[sheet=="sheet2" & year==s$year], script="./www/js/sheet_2.js")
  })

  output$d3_sheet3 = renderD3({
    r2d3(dt()[sheet=="sheet3" & year(year)==s$year], script="./www/js/sheet_3.js")
  })

  output$tb_basin = renderTable(
    hover=T, spacing="xs", colnames=F, align="lr", width="100%",
    melt(as.data.table(ISO3[[s$iso3]])[, `:=`(
      `area` = sprintf("%s ha", comma(area)),
      `population` = sprintf("%s", comma(`population`)),
      `annual rainfall` = sprintf("%s mm", comma(`annual rainfall`)),
      `annual ET` = sprintf("%s mm", comma(`annual ET`)),
      `irrigated area` = sprintf("%s ha", comma(`irrigated area`)),
      `hydro power` = sprintf("%s GWh/year", comma(`hydro power`))
    )], id.vars=1)[!variable %in% c("admin", "water"), .(
      variable = sprintf('<span class="text-info">%s</span>', str_to_title(variable)),
      value)]
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

  observeEvent(input$btnScore, {
    updateNavbarPage(session, "navPage", selected="Scorecard")
  })

  observeEvent(input$btnRefresh, {
    updateNavbarPage(session, "navPage", selected="About WA+")
  })

  observeEvent(input$txtISO3, {
    s$iso3 = tolower(input$txtISO3)
  })

  observeEvent(input$numYear, {
    s$year = ym(input$numYear)
  })

  # Reset map ----
  observeEvent(s$iso3, {
    leafletProxy("map") %>% map_update(s$iso3)
    updateSliderTextInput(session, "numYear", NULL,
      data[iso3==s$iso3 & sheet=="sheet1"][order(year), format(unique(year), "%Y %b")],
      selected=data[, format(max(year), "%Y %b")])
  })

  # Toggle map layers ----
  observeEvent(layers(), {
    leafletProxy("map") %>% map_toggle(layers())
  })

  output$plot_ts = renderHighchart({
    req(s$var$var)
    plot_ts(dt()[id==s$var$var], s$var$color)
  })

  observeEvent(input$bar_clicked, {
    e = input$bar_clicked
    updateTextAreaInput(inputId="objSelected",
      value=paste(s$year, e$var, ": ", comma(as.numeric(e$value), accuracy=0.01)))
    s$var = e
  })

}
