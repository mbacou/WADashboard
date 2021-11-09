#####################################################################################
# Title:   IWMI WA+ Dashboard (UI)
# Date:    Oct 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

# Navbar ----
navbar <- dashboardHeader(
  title = a(class="ml-2 p-1 text-xl text-white",
    href="https://wateraccounting.org/",
    "WATER", strong("Accounting+"),
    span(class="ml-4 text-warning", "DRAFT")
  ),
  skin = "dark",
  status = "primary",
  fixed = TRUE,
  border = FALSE,
  compact = TRUE,
  controlbarIcon = icon("info-circle"),

  rightUi = tagList()
)

# Footer ----
footer <- dashboardFooter(
  fluidRow(class="align-items-end",

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
      ))
  )
)

# Sidebar ----
sidebar <- dashboardSidebar(
  disable = TRUE,
  skin = "dark",
  status = "primary",
  elevation = 0,
  expandOnHover = FALSE
)

# Controlbar ----
controlbar = dashboardControlbar(
  skin = "dark",
  width = "33%",

  controlbarMenu(type="pills",
    controlbarItem("About", icon=icon("info-circle"),
      p("[to be inserted]")
    ),
    controlbarItem("Definitions", icon=icon("info-circle"),
      p("[to be inserted]")
    )
  )
)

# Timeline ----
tab_1 <- tabPanel("Recharge Summary", icon=icon("tint"),
  fluidRow(class="m-2",
    column(8,
      highchartOutput("hcTimeline")
    ),
    column(4, p("more"))
  )
)

# Sheet 1 ----
tab_2 <- tabPanel("Resource Base", icon=icon("th"),
  fluidRow(class="m-2",
    column(8,
      sliderInput("numYear", "Year",
        min=data[, min(year)], max=data[, max(year)],
        value=data[, max(year)], timeFormat="%Y"),
      d3Output("d3", width="100%"),
      textAreaInput("objSelected", "Click a cell to get its value", "none")
    ),
    column(4,
      highchartOutput("plot_ts", height="200px")
    )
  )
)

# Sheet 2 ----
tab_3 <- tabPanel("Evapotranspiration", icon=icon("envira"),
  fluidRow(class="m-2",
    column(3,
      textAreaInput("objSelected", "Click a cell to get its value", "none")
    ),
    column(9
    )
  )
)

tab_21 <- tabPanel("Scorecard", icon=icon("crosshairs"), class="p-0",
  fluidRow(
    column(12,
      fluidRow(
        valueBox(
          span(class="text-gray", "Sustainability score"), "Good",
          icon=icon("leaf"), width=6),
        valueBox(
          span(class="text-gray", "Productivity score"), "Poor",
          icon=icon("check-double"), width=6)
      ),
      uiOutput("ui_Overview", inline=F))
  )
)

tab_22 <- tabPanel("Profile", icon=icon("table"),
  fluidRow(
    column(12, class="table-responsive",
      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur porta
        in dolor in feugiat. Nunc maximus purus dui."),
      tableOutput("tb_basin")
    )
  )
)

tab_23 <- tabPanel("My Summary", icon=icon("draw-polygon"),
  fluidRow(class="m-2",
    column(12,
      p("Upload your area of interest."),
      fileInput("fileGeo", NULL, placeholder="zoi.geojson",
        accept=c(".csv", ".geojson", ".kml", ".shp")),
      actionButton("btnUpload", "Upload")
    )
  )
)

# Filters ----
row_1 <- fluidRow(class="bg-light no-gutters align-items-start",

  # Map ----
  column(7, class="bg-primary",
    fluidRow(class="mx-2 align-items-end",
      column(10,
        p(class="mt-2 text-muted text-emphasis",
          "This dashboard compiles results of a new Water Accounting methodology (WA+)
          based on global-scale public-domain datasets. Its objective is to
          strive to achieve equitable and transparent water governance for all users
          and a sustainable water balance.")
      ),
      column(10,
        pickerInput("txtISO3",
          span(class="text-info h4", "Choose a river basin"),
          choices=names(ISO3), selected=init$iso3,
          options=pickerOptions(style="btn-info"),
          choicesOpt=list(content=l_iso3))
      ),
      column(2,
        pickerInput("txtUnit", span(class="text-info", "Units"),
          c("km³", "ft³", "MCM"),
          options=pickerOptions(style="btn-light"))
      )
    ),
    leafletOutput("map", width="100%", height="25rem")
  ),

  # Overview ----
  column(5,
    fluidRow(class="no-gutters",
      column(7, class="bg-primary pl-2 pt-3 align-bottom",
        h3(class="text-lightblue", "Basin Situation")),
      column(5, class="bg-primary p-2",
        actionButton("btnRefresh",
          span("Last model run", strong(format(init$date, "%Y %b"))),
          icon=icon("refresh"), status="info", width="100%", outline=T)
      ),
      tabBox(id="tabOverview", width=12, type="pills",
        collapsible=F, elevation=0, side="right",
        status="info", solidHeader=T,
        tab_21, tab_22, tab_23)
    )
  )
)


# Tabbox ----
row_2 <- fluidRow(class="no-gutters",
  tabBox(width=12, type="pills", collapsible=F, side="right",
    background="white", status="gray-dark", solidHeader=T,
    tab_1, tab_2, tab_3)
)

# Main layout ----
body <- dashboardBody(
  fresh::use_theme("bs4Dash.css"),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="iwmi.css"),
    tags$link(rel="shortcut icon", href="favicon.ico")
  ),
  add_busy_bar(color=pal[["yellow"]], height="3px"),
  setSliderColor("#5088c6", 1:2),
  row_1, row_2
)

function() {
  dashboardPage(navbar, sidebar, body, controlbar, footer,
    title = "IWMI | Water Accounting+",
    dark = FALSE
  )
}
