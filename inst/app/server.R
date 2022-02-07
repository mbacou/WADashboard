#####################################################################################
# Title:   IWMI WA+ Dashboard (SERVER)
# Date:    October 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

function(input, output, session) {

  session$allowReconnect(TRUE)

  # Global filters
  s = reactiveValues(
    iso3 = init$iso3,
    date = init$date,
    var = list(var=init$var, color="green"),
    layers = NA
  )

  # Data cube slices
  dt = reactive( data[iso3==s$iso3] )
  dtf = reactive( dt()[date_end==s$date] )

  # Observers ----
  observeEvent(input$txtISO3, {
    s$iso3 = tolower(input$txtISO3)
  })

  observeEvent(input$txtDate, {
    # Always last day of selected month
    s$date = ceiling_date(ym(input$txtDate), "months") - days(1)
  })

  observeEvent(s$iso3, {
    # Update map
    leafletProxy("map") %>% map_update(s$iso3)
    # Update timestamp
    updateActionButton(session, "btnRefresh",
      label=sprintf("%s - %s",
        dt()[, format(min(date_start), "%Y %b")], dt()[, format(max(date_end), "%Y %b")]))
    # Update periodicity
    updateRadioGroupButtons(session, "txtPeriod",
      disabledChoices=dt()[sheet=="sheet1", setdiff(c("year", "season", "month"), unique(period))])
  })

  observeEvent(input$txtPeriod, {
    # Update time slider
    dt <- dt()[sheet=="sheet1"]
    updateSliderTextInput(session, "txtDate", NULL,
      dt[order(date_end), format(unique(date_end), "%Y %b")],
      selected=dt[, format(max(date_end), "%Y %b")])
  })

  observeEvent(input$btnScore, {
    updateNavbarPage(session, "navPage", selected="Scorecard")
  })

  observeEvent(input$btnRefresh, {
    updateNavbarPage(session, "navPage", selected="About WA+")
  })


  # Sheets ----
  output$d3_sheet1 = renderD3({
    r2d3(dtf()[sheet=="sheet1"], script="./www/js/sheet_1.js")
  })

  output$d3_sheet2 = renderD3({
    r2d3(dtf()[sheet=="sheet2"], script="./www/js/sheet_2.js")
  })

  output$d3_sheet3 = renderD3({
    r2d3(dtf()[sheet=="sheet3"], script="./www/js/sheet_3.js")
  })

  observeEvent(input$bar_clicked, {
    e = input$bar_clicked
    updateTextAreaInput(inputId="objSelected",
      value=paste(s$date, e$var, ": ", comma(as.numeric(e$value), accuracy=0.01)))
    s$var = e
  })


  # Key facts ----
  output$tb_basin = renderTable(
    hover=T, spacing="xs", colnames=F, align="lr", width="100%", {
      # Flatten list to data.table
      dt = lapply(ISO3[[s$iso3]],
        function(x) if(is.character(x)) paste(x, collapse=", ") else x) %>%
        as.data.table()
      # Format
      dt[, `:=`(
        `authorities` = sprintf(
          '%s <a href="%s"><i class="fa fa-external-link fa-fw"></i></a>', authorities, url),
        `area` = sprintf("%s ha", comma(area)),
        `population` = sprintf("%s", comma(`population`)),
        `annual rainfall` = sprintf("%s mm", comma(`annual rainfall`)),
        `annual ET` = sprintf("%s mm", comma(`annual ET`)),
        `irrigated area` = sprintf("%s ha", comma(`irrigated area`)),
        `hydro power` = sprintf("%s GWh/year", comma(`hydro power`))
      )]
      melt(dt, id.vars=1)[!variable %in% c("country", "admin", "water", "source", "url", "unit"), .(
        variable = sprintf('<span class="text-info">%s</span>', str_to_title(variable)),
        value
      )]
    }
  )

  # Map ----
  output$map = renderLeaflet( map_init(init$iso3) %>%
      map_addWMSProvider(provider="FAO", date=init$date)
  )

  # Toggle map layers
  observe({
    s$layers = c(input$chkLayer_1, input$chkLayer_2, input$chkLayer_3)
  })

  observeEvent(s$layers, ignoreInit=TRUE, {
    leafletProxy("map") %>% map_toggle(provider="FAO", layers=s$layers)
  })

  # Toggle layer timestamp
  observeEvent(s$date, {
    req(length(s$layers) > 0)
    leafletProxy("map") %>%
      map_addWMSProvider(provider="FAO", date=s$date) %>%
      map_toggle(provider="FAO", layers=s$layers)
  })

  output$uiLegend = renderUI(
    if(length(s$layers) > 0) lapply(s$layers, function(x)
      tagList(h5(class="text-info", x), img(class="img-responsive",
        src=sprintf(LAYERS[["FAO"]]$legend, LAYERS[["FAO"]]$layers[[x]]))
      )) else p(class="mt-2 text-muted", "No layer selected.")
  )

  output$uiInfo = renderUI(
    if(length(s$layers) > 0) lapply(s$layers, function(x)
      tagList(h5(class="text-info", x),
        LAYERS[["FAO"]]$info
      )) else p(class="mt-2 text-muted", "No layer selected.")
  )


  # Cards ----
  output$ui_score_prod = renderUI({
    dt = fread("
    variable, value, status, max
    Net Inflow, 12, warning, 60
    Depleted Water, 79, success, 120
    Anthropogenic Uses, 30, danger, 110
    Available for Allocation, 4, danger, 20
      ")
    lapply(1:nrow(dt), function(x) dt[x,
      progressBar(paste0("pbg-", x), value, total=max,
        title=span(class="pt-3", variable), status=status, display_pct=T)]
    ) %>% tagList()
  })

  output$ui_score_sust = renderUI({
    p("[10-year trend radar chart]")
  })


  # Plots ----
  output$plot_ts = renderHighchart({
    req(s$var$var)
    plot_ts(dt()[id==s$var$var], s$var$color)
  })

  output$plot_gauge = renderHighchart({
    plot_gauge(iso3="ken")
  })

  output$plot_radar = renderHighchart({
    plot_radar(iso3=s$iso3)
  })

}
