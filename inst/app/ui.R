#####################################################################################
# Title:   IWMI WA+ Dashboard (UI)
# Date:    Oct 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

# Footer ----
footer <- fluidRow(class="align-items-end",
  column(9,
    a(href="https://iwmi.cgiar.org/",
      img(class="mx-3", height="50px", src="./fig/iwmi_logo_w.svg")),
    a(href="https://cgiar.org/",
      img(class="mx-3", height="60px", src="./fig/cgiar_w.png")),
    a(href="https://fao.org/",
      img(class="mx-3", height="50px", src="./fig/fao_logo_w.svg")),
    a(href="https://en.unesco.org/wwap/",
      img(class="mx-3", height="50px", src="./fig/wwap_w.png"))
  ),
  column(3, class="pr-3 text-right",
    p(a(class="text-white", "Terms of use",
      href="https://www.iwmi.cgiar.org/about/legal-information/"), br(),
      HTML("&copy; IWMI"),
      paste(year(Sys.Date()), "All rights reserved.", sep=". ")
    ),
    p("Version",
      as.character(packageVersion("WADashboard")[1]),
      "(", a(class="text-white",
        href="https://mbacou.github.io/WADashboard/news", "what's new"), ")"
    )
  )
)

# Scorecards ----
tab_11 <- tabPanel(span("Water", br(), "Productivity"), class="p-0",
  p("[placeholder]", br(), "
        Which scoring dimensions and key indicators / measures to highlight in this section?
        "),
  fluidRow(
    valueBox(
      span(class="text-gray", "Score 1"), "Good",
      icon=icon("tint"), width=6),
    valueBox(
      span(class="text-gray", "Score 2"), "Poor",
      icon=icon("check-double"), width=6)
  ),
  uiOutput("ui_Overview", inline=F),
  div(class="text-right",
    actionButton("btnScore1", "Details", width="6rem", status="secondary")
  )
)

tab_12 <- tabPanel(span("Sustainability", br(), "Score"), class="p-0",
  p("[placeholder]", br(), "
        Which scoring dimensions and key indicators / measures to highlight in this section?
        "),
  fluidRow(
    valueBox(
      span(class="text-gray", "Score 1"), "Good",
      icon=icon("leaf"), width=6),
    valueBox(
      span(class="text-gray", "Score 2"), "Poor",
      icon=icon("check-double"), width=6)
  ),
  #uiOutput("ui_Overview", inline=F),
  div(class="text-right",
    actionButton("btnScore2", "Details", width="6rem", status="secondary")
  )
)


# Profile ----
tab_13 <- tabPanel(span("Basin", br(), "Profile"), icon=icon("table"),
  div(class="table-responsive",
    p("[placeholder]", br(), "
        Which key basin features / characteristics to highlight in this section?
        "),
    tableOutput("tb_basin"),
    div(class="text-right",
      actionButton("btnProfile", "Details", width="6rem", status="secondary")
    )
  )
)

# Filters ----
row_1 <- fluidRow(class="bg-gradient-light no-gutters align-items-end",

  # Map ----
  column(7,
    fluidRow(class="mx-2 align-items-end",
      column(12,
        p(class="my-4 mr-2 text-muted font-italic",
          "This dashboard compiles results of a new",
          a(class="text-gray-dark", href="https://wateraccounting.org/",
            "Water Accounting"),
          " methodology (WA+) based on global-scale public-domain datasets. Its
          objective is to achieve equitable and transparent water governance for
          all water consumers and ensure a sustainable water balance.")
      ),
      column(10,
        pickerInput("txtISO3",
          span(class="text-info h4", "Choose a river basin"),
          choices=names(ISO3), selected=init$iso3,
          options=pickerOptions(style="btn-white"),
          choicesOpt=list(content=l_iso3))
      ),
      column(2, class="text-right",
        pickerInput("txtUnit", span(class="text-info", "Units"),
          c("km³", "ft³", "MCM"),
          options=pickerOptions(style="btn-white"))
      )
    ),
    leafletOutput("map", width="100%", height="22rem")
  ),

  # Overview ----
  column(5,
    fluidRow(class="no-gutters mt-4 align-items-end",
      column(7, class="pl-2",
        h3(class="text-lightblue", "Basin Situation")),
      column(5, class="p-2",
        actionButton("btnRefresh",
          span("Last model run", strong(format(init$date, "%Y %b"))),
          icon=icon("sync"), status="info", width="100%", outline=T, size="sm")
      ),
      tabBox(id="boxOverview", width=12, type="pills",
        collapsible=F, elevation=0, side="right",
        background="white", status="info", solidHeader=T,
        tab_11, tab_12, tab_13
      )
    )
  )
)

# Timeline ----
tab_21 <- tabPanel("Recharge and Abstraction", icon=icon("tint"),
  fluidRow(class="m-2",
    column(8,
      highchartOutput("hcTimeline")
    ),
    column(4, p("more"))
  )
)

# Sheet 1 ----
tab_22 <- tabPanel("Resource Base", icon=icon("th"),
  fluidRow(class="m-2",
    column(8,
      sliderInput("numYear", "Year",
        min=data[, min(year)], max=data[, max(year)],
        value=data[, max(year)], timeFormat="%Y"),
      d3Output("d3", width="100%"),
      p(), p(),
      textAreaInput("objSelected", "Click a cell to get its value", "none")
    ),
    column(4,
      highchartOutput("plot_ts", height="200px")
    )
  )
)

# Sheet 2 ----
tab_23 <- tabPanel("Evapotranspiration", icon=icon("envira"),
  fluidRow(class="m-2",
    column(3,
      textAreaInput("objSelected", "Click a cell to get its value", "none")
    ),
    column(9
    )
  )
)


# Page 1 ----
page_1 <- fluidRow(class="no-gutters",
  tabBox(width=12, type="pills", collapsible=F, side="right",
    background="white", status="gray-dark", solidHeader=T,
    tab_21, tab_22, tab_23)
)

# Page 2 ----
page_2 <- fluidRow(
  column(12,
    p("[placeholder content for detailed scorecard and basin profile.]")
  )
)

# Page 3 ----
page_3 <- fluidRow(class="mt-3",
  column(6,
    box(title="My Summary", width=12, icon=icon("draw-polygon"),
      elevation=0, collapsible=F, background="white", status="gray-dark", solidHeader=T,
      p("[placeholder]", br(), "
        Allow users to provide a custom area to summarize over and to select a list of
        indicators to include in a custom report.
        "),
      pickerInput("txtAdmin",
        span(class="text-info mb-2", "Choose a subdivision..."),
        l_admin(init$iso3), width="240px"),
      fileInput("fileGeo",
        span(class="text-info", "... or upload a zone of interest"),
        placeholder="zoi.geojson", accept=c(".csv", ".geojson", ".kml", ".shp"),
        width="240px"),
      checkboxGroupInput("txtReport",
        span(class="text-info mb-2", "Water variables to include:"),
        names(SOURCES[["FAO-DATA"]]$layers)[-c(1:2)]),
      actionButton("btnUpload", "Upload")
    )
  ),
  column(6,
    p("[preview]")
  )
)

function() {
  navbarPage(
    theme = bslib::bs_theme(version="4"),
    windowTitle = "IWMI | Water Accounting+",
    title = a(class="text-primary",
      href="https://wateraccounting.org/",
      "WATER", strong("Accounting+"),
      span(class="ml-4 text-warning", "DRAFT")
    ),
    position = "fixed-top",
    collapsible = TRUE,
    header = tagList(
      tags$head(
        #tags$link(rel="stylesheet", type="text/css", href="iwmi.css"),
        tags$link(rel="shortcut icon", href="favicon.ico")
      ),
      setSliderColor(pal[["blue"]], 1:2),
      row_1
    ),
    footer = footer,
    selected = "Overview",
    tabPanel("Overview", page_1),
    tabPanel("Scorecard", page_2),
    tabPanel("My Summary", page_3)
  )
}
