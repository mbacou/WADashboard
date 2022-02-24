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
      "This dashboard summarizes results from the Water Accounting+ hydrological model
      based on global-scale public-domain datasets. WA+ program objective is to
      achieve equitable and transparent water governance for all water consumers and
      to ensure a sustainable water balance."),
    p("The WA+ framework is developed by the UNESCO-IHE,
      Delft in partnership with the International Water Management Institute, the
      Food and Agriculture Organization of the United Nations, and the World Water
      Assessment Program."),
    p(class="small",
      HTML("&copy; IWMI"),
      paste(year(Sys.Date()), "All rights reserved.", sep=". "),
      a(class="text-white", "Terms of use.",
        href="https://www.iwmi.cgiar.org/about/legal-information/"),
      br(), "Version",
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
header <- fluidRow(class="pt-5 align-items-end shadow",
  column(8,
    h3(class="text-primary", "Water Accounting+ for",
      em("better"), "water resource management.")
  ),
  column(5,
    pickerInput("txtISO3",
      span(class="text-info", "Select a river basin"),
      choices=names(ISO3), selected=init$iso3, width="16rem",
      options=pickerOptions(style="btn-white"),
      choicesOpt=list(content=l_iso3()))
  ),
  # Filters
  column(7,
    fluidRow(class="no-gutters float-md-right align-items-end",
      div(class="col pb-3",
        tags$label(class="text-info", "Timespan"), br(),
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
  class="w-100 no-gutters waved border-top border-bottom align-items-center",
  column(12, class="px-4 pt-2 pb-0",
    div(class="float-left", "Basin Timeline"),
    sliderTextInput("txtDate", NULL,
      data[iso3==init$iso3 & sheet=="sheet1"
      ][order(date_end), format(unique(date_end), "%Y %b")],
      selected=format(init$date, "%Y %b"),
      width="99%", grid=TRUE, hide_min_max=TRUE)
  )
)

# Map ----
map <- fluidRow(id="divMap", class="w-100 no-gutters collapse show",
  column(8, style="height:22rem;",
    leafletOutput("map", width="100%", height="100%")
  ),
  column(4, class="waved3",
    navs_pill(
      nav(title=span(class="small", "Layers"), icon=icon("layer-group"),
        fluidRow(class="no-gutters",
          column(12,
            style="height:19.2rem; overflow:auto;",
            accordion(id="accLayers",
              # Pull FAO layers for now
              accordionItem(
                title=names(LAYERS[["FAO"]]$layers)[2],
                icon=icon("caret-right"),
                class="border-0", bg="white", collapsed=FALSE,
                awesomeCheckboxGroup("chkLayer_2", NULL,
                  choices=names(LAYERS[["FAO"]]$layers[[2]]),
                  status="info", width="100%")
              ),
              accordionItem(
                title=names(LAYERS[["FAO"]]$layers)[3],
                icon=icon("caret-right"),
                class="border-0", bg="white", collapsed=TRUE,
                awesomeCheckboxGroup("chkLayer_3", NULL,
                  choices=names(LAYERS[["FAO"]]$layers[[3]]),
                  width="100%", status="info")
              ),
              accordionItem(
                title=names(LAYERS[["FAO"]]$layers)[4],
                icon=icon("caret-right"),
                class="border-0", bg="white", collapsed=TRUE,
                awesomeCheckboxGroup("chkLayer_4", NULL,
                  choices=names(LAYERS[["FAO"]]$layers[[4]]),
                  width="100%", status="info")
              )
            )
          )
        )
      ),
      nav(title=span(class="small", "Legend"), icon=icon("palette"),
        column(12, class="bg-white",
          style="height:19.2rem; overflow:auto;",
          uiOutput("uiLegend"))
      ),
      nav(title=span(class="small", "Info"), icon=icon("info-circle"),
        column(12, class="pb-2 bg-white",
          style="height:19.2rem; overflow:auto;",
          uiOutput("uiInfo"))
      )
    )
  )
)


# Scorecards ----
tab_11 <- tagList(
  h3(class="text-primary", "Key Facts"),
  div(class="table-responsive waved2", tableOutput("tb_basin"))
)

tab_12 <- tagList(
  h3(class="text-primary", "Water Availability"),
  highchartOutput("plot_gauge", height="340px")
)

tab_13 <- tagList(
  h3(class="text-primary", "Basin Variability"),
  highchartOutput("plot_radar", height="360px"),
  div(class="text-right",
    actionButton("btnScore", "Learn More", width="6rem", class="my-3")
  )
)


# Overview ----
overview <- fluidRow(
  column(4, tab_11),
  column(3, tab_12),
  column(5, tab_13)
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
      includeMarkdown("./md/sheet_1.md")
    ),
    column(8,
      d3Output("d3_sheet1", width="100%"),
      highchartOutput("plot_ts", height="200px")
    )
  )
)

# Sheet 2 ----
sheet_2 <- nav("Evapotranspiration", icon=icon("envira"),
  fluidRow(class="bg-white",
    column(4,
      includeMarkdown("./md/sheet_2.md")
    ),
    column(8,
      d3Output("d3_sheet2", width="100%")
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
  column(12, overview, timeline)
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
    footer=column(12,
      textAreaInput("objSelected", "Click a cell to get its value", "none")
    ),
    nav_spacer(), sheet_1, sheet_2, sheet_3)
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
page_5 <- fluidRow(class="border-top",
  column(12,
    includeMarkdown("./md/about.md"))
)


# Layout ----
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
        tags$link(rel="shortcut icon", href="fig/favicon.ico")
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
