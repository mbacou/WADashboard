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

  dt = reactive(
    data[iso3==s$iso3]
  )

  # Map
  output$map = renderLeaflet(lmap_init(init$iso3))

  # Sheet 1
  output$d3 = renderD3({
    r2d3(dt()[sheet=="sheet1" & year(year)==s$year], script="./www/js/sheet_1.js")
  })

  output$tb_basin = renderTable(
    hover=T, spacing="xs", colnames=F, align="lr", width="100%",
    fread('
    label, "value"
    Area, "40,000 km²"
    Population, "800,000"
    Countries, "4"
    Authorities, "Niger Basin Authority"
    Annual rainfall, "600 mm"
    Annual ET, "400 mm"
    Major Season, "Apr-Sep"
    Minor Season, "Oct-Dec"
    Irrigated, "400 ha"
    Major Dams, "5"
    Hydro power generation, "50 MW"
      ')[, label := sprintf('<span class="text-info">%s</span>', label)]
  )

  output$ui_Overview = renderUI({
    dt = fread("
    variable, value, status, max
    Utilizable Flow, 79, success, 120
    Blue water availability, 30, danger, 110
    Green water availability, 12, warning, 60
      ")
    lapply(1:nrow(dt), function(x) dt[x,
      tagList(
        p(HTML(sprintf('%s <span class="float-right">%s / %s km³</span>',
          variable, comma(value), comma(max)))),
        bs4ProgressBar(value, label=NULL, status=status, animated=T, size="sm"))]
    ) %>% tagList()
  })

  observeEvent(input$txtISO3, {
    s$iso3 = tolower(input$txtISO3)
  })

  observeEvent(input$numYear, {
    s$year = year(input$numYear)
  })

  observeEvent(s$iso3, {
    leafletProxy("map") %>% lmap_update(s$iso3)
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
