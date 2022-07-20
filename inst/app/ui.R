#####################################################################################
# Title:   IWMI WA+ Dashboard (UI)
# Date:    Oct 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

# Footer ----
footer <- fluidRow(class="bg-dark",
  column(7,
    p(class="mt-3", span(class="lead text-warning", "ABOUT"), br(),
      "This dashboard compiles results from the Water Accounting+ hydrologic model
      based on global-scale public-domain datasets. WA+ objective is to
      achieve equitable and transparent water governance for all water consumers and
      to ensure a sustainable water balance."),
    p("The WA+ framework is developed by the UNESCO-IHE,
      Delft in partnership with the International Water Management Institute, the
      Food and Agriculture Organization of the United Nations, and the World Water
      Assessment Program."),
    p(HTML("&copy; IWMI"),
      paste(year(Sys.Date()), "all rights reserved", sep=", "),
      HTML("&ndash;"),
      a(class="text-white", "Terms of use",
        href="https://www.iwmi.cgiar.org/about/legal-information/"),
      HTML("&ndash;"), "Version",
      as.character(packageVersion("WADashboard")[1]),
      "(", a(class="text-white",
        href="https://mbacou.github.io/WADashboard/news/", "what's new"), ")"
    )
  ),
  column(4, offset=1,
    p(class="mt-3 mr-3 text-md-right",
      a(href="https://iwmi.cgiar.org/",
        img(class="m-3", height="66px", src="./fig/iwmi_w.png")),
      a(href="https://www.un-ihe.org/",
        img(class="m-3", height="64px", src="./fig/ihe_logo.png")),
      br(),
      a(href="https://fao.org/",
        img(class="m-3", height="50px", src="./fig/fao_logo_w.svg")),
      a(href="https://en.unesco.org/wwap/",
        img(class="m-3", height="50px", src="./fig/wwap_w.png"))
    )
  )
)

# Header ----
header <- fluidRow(class="pt-5 align-items-end bg-white shadow",
  column(8,
    h3(class="text-primary", "Water Accounting+ for",
      em("better"), "water resource management.")
  ),
  column(5,
    pickerInput("txtISO3",
      span(class="text-info", "River basin"),
      choices=names(ISO3), selected=init$iso3, width="17rem",
      options=pickerOptions(style="btn-outline-info"),
      choicesOpt=list(content=l_iso3()))
  ),
  # Filters
  column(7,
    fluidRow(class="no-gutters float-md-right align-items-end",
      div(class="col pb-3",
        tags$label(class="text-info", "Modeled timespan"), br(),
        actionButton("btnRefresh", "-",
          class="btn-outline-info btn-sm", width="9rem")
      ),
      div(class="col pl-2",
        radioGroupButtons("txtPeriod",
          span(class="text-info", "Periodicity"), c("year", "season", "month"),
          status="outline-info", justified=TRUE, size="sm")
      ),
      div(class="col pl-2",
        radioGroupButtons("txtUnit",
          span(class="text-info", "Volume units"), c("km³", "ft³", "MCM"),
          status="outline-info", justified=TRUE, size="sm", width="8rem")
      ),
      div(class="col pl-2 pb-3",
        actionButton("btnMap", NULL, icon=icon("globe"), class="btn-outline-info btn-sm",
          `data-toggle`="collapse", `data-target`="#divMap",
          `aria-expanded`="true", `aria-controls`="divMap")
      )
    )
  )
)

# Slider ----
slider <- fluidRow(
  class="w-100 no-gutters bg-waved2 border-top border-bottom align-items-center",
  column(12, class="px-4 pt-2 pb-0",
    div(class="float-left text-bold", "Basin Timeline"),
    sliderTextInput("txtDate", NULL,
      choices=data[iso3==init$iso3 & sheet=="sheet1"
      ][order(date_end), format(unique(date_end), "%Y %b")],
      selected=format(init$date, "%Y %b"),
      width="99%", grid=TRUE, hide_min_max=TRUE)
  )
)

# Map ----
layers <- accordion(id="accLayers",
  # Pull FAO layers for now
  accordionItem(
    title=span(names(LAYERS[["FAO"]]$layers)[2],
      span(class="float-right small text-muted",
        length(LAYERS[["FAO"]]$layers[[2]]), "layers")),
    icon=icon("caret-right"),
    class="border-0", bg="white", collapsed=FALSE,
    awesomeCheckboxGroup("chkLayer_2", NULL,
      choices=names(LAYERS[["FAO"]]$layers[[2]]),
      status="info", width="100%")
  ),
  accordionItem(
    title=span(names(LAYERS[["FAO"]]$layers)[3],
      span(class="float-right small text-muted",
        length(LAYERS[["FAO"]]$layers[[3]]), "layers")),
    icon=icon("caret-right"),
    class="border-0", bg="white", collapsed=TRUE,
    awesomeCheckboxGroup("chkLayer_3", NULL,
      choices=names(LAYERS[["FAO"]]$layers[[3]]),
      width="100%", status="info")
  ),
  accordionItem(
    title=span(names(LAYERS[["FAO"]]$layers)[4],
      span(class="float-right small text-muted",
        length(LAYERS[["FAO"]]$layers[[4]]), "layers")),
    icon=icon("caret-right"),
    class="border-0", bg="white", collapsed=TRUE,
    awesomeCheckboxGroup("chkLayer_4", NULL,
      choices=names(LAYERS[["FAO"]]$layers[[4]]),
      width="100%", status="info")
  )
)

map <- fluidRow(id="divMap", class="w-100 no-gutters collapse show",
  column(8, style="height:22rem;",
    leafletOutput("map", width="100%", height="100%")
  ),
  column(4, class="bg-waved3",
    navs_pill(
      # Layers
      nav(title="Layers", icon=icon("layer-group"),
        fluidRow(class="no-gutters",
          column(12,
            style="height:19.2rem; overflow:auto;",
            layers)
        )
      ),
      # Layer legend
      nav(title="Legend", icon=icon("palette"),
        column(12, class="bg-white",
          style="height:19.2rem; overflow:auto;",
          uiOutput("uiLegend"))
      ),
      # Layer info
      nav(title="Layer Info", icon=icon("info-circle"),
        column(12, class="pb-2 bg-white",
          style="height:19.2rem; overflow:auto;",
          uiOutput("uiInfo"))
      )
    )
  )
)

# Overview ----
overview <- fluidRow(class="bg-white",
  column(4,
    h4(class="text-primary", "Overview"),
    uiOutput("txt_desc"),
    h4(class="text-primary", "Key Facts"),
    div(class="table-responsive bg-waved2", tableOutput("tb_basin")),
    h4(class="text-primary", "Land Use"),
    highchartOutput("plot_luc", height="280px"),
    p()
  ),
  column(8, class="bg-waved",
    fluidRow(
      column(12, h4(class="text-primary", "Water Availability")),
      valueBoxSpark(
        data[iso3=="ken" & id=="net_inflow" & period=="year", .(year, value)],
        title="Basin Closure",
        info="When supply of water falls short of commitments to fulfil demand
        in terms of water quality and quantity within the basin and at the river
        mouth, for part or all of the year, basins are said to be closing.",
        selected=2017, width=4, type="area", unit="%"),
      valueBoxSpark(
        data[iso3=="ken" & id=="landsc_et" & period=="year", .(year, value)],
        title="Availability per Capita",
        width=4, type="column", unit=" km³"),
      valueBoxSpark(
        data[iso3=="ken" & id=="utilized_flow" & period=="year", .(year, value)],
        title="Available for further Use",
        width=4, type="area", unit=" km³")
    ),
    fluidRow(
      column(8, h4(class="text-primary", "Water Uses")),
      column(4, h4(class="text-primary", "Basin Variability")),
      valueBoxSpark(0, title="Agricultural Water Use",
        width=4),
      valueBoxSpark(
        data[iso3=="ken" & id=="reserved_outflow" & period=="year", .(year, value)],
        title="Environmental Stress",
        width=4, type="area", unit="%"),
      valueBoxSpark(
        data[iso3=="ken" & id=="rainfall" & period=="year", .(year, value)],
        title="Precipitation",
        selected=2017, width=4, type="area", unit=" km³")
    )
  )
)

# Timeline ----
timeline <- fluidRow(class="bg-white",
  column(8,
    h4(class="text-primary", "Recharge and Abstraction"),
    p("[Time-series and anomalies]"),
    highchartOutput("plot_timeline")
  ),
  column(4, p("more"))
)

# Sheet 1 ----
sheet_1 <- nav("Resource Base", icon=icon("th"),
  fluidRow(class="bg-white",
    column(4,
      p(),
      includeMarkdown("./md/sheet_1.md"),
      highchartOutput("plot_ts_s1", height="200px")
    ),
    column(8,
      d3Output("d3_sheet1", height="500px")
    )
  )
)

# Sheet 2 ----
sheet_2 <- nav("Evapotranspiration", icon=icon("envira"),
  fluidRow(class="bg-white",
    column(4,
      p(),
      includeMarkdown("./md/sheet_2.md"),
      highchartOutput("plot_ts_s2", height="200px")
    ),
    column(8,
      d3Output("d3_sheet2", height="500px")
    )
  )
)

# Sheet 3 ----
sheet_3 <- nav("Agricultural Services", icon=icon("faucet"),
  fluidRow(class="bg-white",
    column(4,
      includeMarkdown("./md/sheet_3.md")
    ),
    column(8,
      d3Output("d3_sheet3", width="100%")
    )
  )
)


# Page 1 ----
page_1 <- fluidRow(
  column(12, overview)
)

# Page 2 ----
page_2 <- fluidRow(
  column(12,
    h4(class="text-primary", "Water Cycle"),
    p("[placeholder content for water flux dynamics]")
  )
)

# Page 3 ----
page_3 <- fluidRow(class="d-block bg-white",
  navs_bar(
    title = span(class="h4 text-primary", "Water Accounts"),
    bg = "transparent",
    inverse = FALSE,
    nav_spacer(), sheet_1, sheet_2)
)

# Page 4 ----
page_4 <- fluidRow(
  column(6,
    h4(class="text-primary", "My Area"),
    p("[placeholder]", br(), "
        Allow users to provide a custom area to summarize over and to select a list of
        indicators to include in a custom report.
        "),
    checkboxGroupInput("txtReport",
      span(class="text-info mb-2", "Water variables to include:"),
      names(LAYERS[["FAO-DATA"]]$layers)[-c(1:2)]),
    pickerInput("txtAdmin",
      span(class="text-info mb-2", "Choose a subdivision..."),
      l_admin(init$iso3), width="240px"),
    fileInput("fileGeo",
      span(class="text-info", "... or upload a zone of interest"),
      placeholder="zoi.geojson", accept=c(".csv", ".geojson", ".kml", ".shp"),
      width="240px"),
    actionButton("btnUpload", "Upload"),
    p()
  ),
  column(6,
    p("[preview]")
  )
)

# Page 5 ----
page_5 <- fluidRow(class="d-block bg-white",
  navs_bar(
    title = span(class="h4 text-primary", "About WA+"),
    bg = "transparent",
    inverse = FALSE,
    nav_spacer(),
    nav("Modeling Approach", icon=icon("info-circle"),
      column(12,
        includeMarkdown("./md/about.md"))
    ),
    nav("Data Sources", icon=icon("database"),
      column(12,
        includeMarkdown("./md/sources.md"),
        div(class="table-responsive bg-waved2", tableOutput("tb_sources"))
      )
    )
  )
)


# Main Layout ----
function() {
  page_navbar(
    id = "navPage",
    theme = bs_themed(),
    window_title = "IWMI | Water Accounting+",
    title = tagList(
      span(class="display-4 text-muted", "WA+ Dashboard"),
      span(class="mx-4 text-warning", "DRAFT")
    ),
    bg = alpha("white", .95),
    position = "fixed-top",
    fluid = TRUE,
    header = tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="extra.css"),
        tags$link(rel="shortcut icon", href="fig/favicon.ico"),
        tags$script(HTML("setInterval(function(){ $('[title]').tooltip(); }, 1000)"))
      ),
      column(12, header), map, slider),
    footer = column(12, footer),
    nav_spacer(),
    nav("Overview", page_1, icon=icon("home")),
    nav("Water Cycle", page_2, icon=icon("redo")),
    nav("Water Accounts", page_3, icon=icon("th")),
    #nav("My Area", page_4, icon=icon("user-cog")),
    nav("About WA+", page_5, icon=icon("info-circle")),
    selected = "Overview"
  )
}
