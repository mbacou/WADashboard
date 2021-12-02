#####################################################################################
# Title:   IWMI WA+ Dashboard (UI)
# Date:    Oct 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

# Footer ----
footer <- fluidRow(
  class="bg-dark align-items-center",
  column(8,
    a(href="https://iwmi.cgiar.org/",
      img(class="m-3", height="50px", src="./fig/iwmi_logo_w.svg")),
    a(href="https://cgiar.org/",
      img(class="m-3", height="60px", src="./fig/cgiar_w.png")),
    a(href="https://fao.org/",
      img(class="m-3", height="50px", src="./fig/fao_logo_w.svg")),
    a(href="https://en.unesco.org/wwap/",
      img(class="m-3", height="50px", src="./fig/wwap_w.png"))
  ),
  column(4, class="p-3 text-right",
    p(a(class="text-white", "Terms of use",
      href="https://www.iwmi.cgiar.org/about/legal-information/"), br(),
      HTML("&copy; IWMI"),
      paste(year(Sys.Date()), "All rights reserved.", sep=". "),
      br(), "Version",
      as.character(packageVersion("WADashboard")[1]),
      "(", a(class="text-white",
        href="https://mbacou.github.io/WADashboard/news", "what's new"), ")"
    )
  )
)

# Scorecards ----
tab_11 <- nav(
  span("Water", br(), "Productivity"),
  p("[placeholder]", br(), "
        Which scoring dimensions and key indicators / measures to highlight in this section?
        "),
  fluidRow(
    scoreBox("Score 1", "Good",
      icon=icon("faucet"), footer="Indicator 1", width=6, color="success"),
    scoreBox("Score 2", "Poor",
      icon=icon("check-double"), footer="Indicator 2", width=6, color="warning")
  ),
  p(),
  uiOutput("ui_score_prod", inline=F)
)

tab_12 <- nav(
  span("Sustainability", br(), "Score"),
  p("[placeholder]", br(), "
        Which scoring dimensions and key indicators / measures to highlight in this section?
        "),
  fluidRow(
    scoreBox("Score 1", "Good",
      icon=icon("tint"), footer="Indicator 1", width=6, color="success"),
    scoreBox("Score 2", "Poor",
      icon=icon("check-double"), footer="Indicator 2", width=6, color="danger")
  ),
  p(),
  uiOutput("ui_score_sust", inline=F)
)


# Profile ----
tab_13 <- nav(
  span("Basin", br(), "Profile"), icon=icon("table"),
  div(class="table-responsive",
    p("[placeholder]", br(), "
        Which key basin features / characteristics to highlight in this section?
        "),
    tableOutput("tb_basin")
  )
)

# Filters ----
intro <- fluidRow(class="pt-5 pb-3 bg-light align-items-start",
  column(8,
    p(class="text-muted",
      "This dashboard compiles results from the",
      a(class="text-gray-dark", href="https://wateraccounting.org/",
        "Water Accounting+"),
      "method based on global-scale public-domain datasets. Its
      objective is to achieve equitable and transparent water governance for
      all water consumers and to ensure a sustainable water balance.")
  ),
  column(3, offset=1, class="text-right",
    actionButton("btnRefresh",
      span("Last model run", strong(format(init$date, "%Y %b"))),
      icon=icon("sync"), class="btn-outline-info btn-sm", width="12rem")
  )
)

filters <- fluidRow(class="bg-light align-items-end",
  column(8,
    pickerInput("txtISO3",
      span(class="h4 text-info", "Choose a river basin"),
      choices=names(ISO3), selected=init$iso3, width="18rem",
      options=pickerOptions(style="btn-white"),
      choicesOpt=list(content=l_iso3))
  ),
  column(4,
    div(class="float-right text-right",
      pickerInput("txtUnit", span(class="text-info", "Units"), width="6rem",
        c("km³", "ft³", "MCM"),
        options=pickerOptions(style="btn-white btn-sm"))
    )
  )
)

# Map ----
map <- leafletOutput("map", width="100%", height="20rem")

# Overview ----
overview <- fluidRow(
  column(4,
    h3(class="text-info", "Basin Profile"),
    tab_13),
  column(4,
    h3(class="text-info", "Water Productivity"),
    tab_11),
  column(4,
    h3(class="text-info", "Sustainability Score"),
    tab_12),
  column(12, class="text-right",
    actionButton("btnScore", "Learn More", width="6rem", class="my-3")
  )
)

# Timeline ----
timeline <- fluidRow(class="bg-white",
  column(8,
    h4(class="text-info", icon=icon("tint"), "Recharge and Abstraction"),
    highchartOutput("hcTimeline")
  ),
  column(4, p("more"))
)

# Sheet 1 ----
sheet_1 <- nav("Resource Base", icon=icon("th"),
  fluidRow(class="bg-white",
    column(4,
      p(),
      markdown("
      The **Resource Base** sheet provides an overview on over‑exploitation,
      unmanageable, manageable, exploitable, reserved, utilized and utilizable
      flows at river basin scale. It is used to:

      - Discern between landscape ET (from rainfall) and incremental ET (from natural
      and manmade withdrawals)
      - Assess commitments to environment and legal agreements
      - Quantify atmospheric water recycling
      - Understand water scarcity during dry years.
        ")
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
      p(),
      markdown("
      The purpose of the **Evapotranspiration** is to:

      - Quantify water consumption for all land use classes throughout the entire
      water basin
      - Describe the anthropogenic impact on ET and concepts of ET management to
      reduce total water consumption from withdrawals and inundations
      - Understand the impact of land use planning on consumptive use
      - Relate water consumption to intended processes (beneficial vs.
        non-beneficial ET).
        ")
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
      p(),
      markdown("
      The purpose of the **Agricultural Services** is to:

      - Assess agricultural yields for food, feed, timber and fish products
      - Compute the related water productivity (kg/m³) and the gap to demonstrate
      loss of returns in volume or value terms
      - Decide on future rainfed and irrigated cropping systems
      - Identify opportunities for saving water in agriculture.
        ")
    ),
    column(8,
      d3Output("d3_sheet3", width="100%")
    )
  )
)


# Page 1 ----
page_1 <- fluidRow(style="display:block",
  column(12, overview, timeline)
)

# Page 2 ----
page_2 <- fluidRow(style="min-height:20rem;",
  column(12,
    h4(class="text-info", "Scorecards"),
    p("[placeholder content for detailed scorecard and basin profile]")
  )
)

# Page 3 ----
page_3 <- fluidRow(style="display:block;",
  navs_bar(
    title="Accounting Sheets", bg=pal[["black"]],
    header=column(12,
      sliderTextInput("numYear", NULL,
        data[, year(seq(min(year), max(year), by="year"))],
        selected=data[, year(max(year))], width="100%", grid=TRUE)
    ),
    footer=column(12,
      textAreaInput("objSelected", "Click a cell to get its value", "none")
    ),
    nav_spacer(), sheet_1, sheet_2, sheet_3)
)

# Page 4 ----
page_4 <- fluidRow(
  column(6,
    h4(class="text-info", "My Area"),
    p("[placeholder]", br(), "
        Allow users to provide a custom area to summarize over and to select a list of
        indicators to include in a custom report.
        "),
    checkboxGroupInput("txtReport",
      span(class="text-info mb-2", "Water variables to include:"),
      names(SOURCES[["FAO-DATA"]]$layers)[-c(1:2)]),
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
page_5 <- fluidRow(style="min-height:20rem",
  column(12,
    h4(class="text-info", "About WA+"),
    p("[About]")
  )
)

function() {
  page_navbar(
    id = "navPage",
    theme = bs_themed(),
    window_title = "IWMI | Water Accounting+",
    title = tagList(
      span(class="h4 text-primary", "WATER", strong("Accounting+")),
      span(class="mx-4 text-warning", "DRAFT")
    ),
    bg = alpha(pal[["light"]], .9),
    position = "fixed-top",
    header = tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="iwmi.css"),
        tags$link(rel="shortcut icon", href="favicon.ico")
      ),
      setSliderColor(pal[["blue"]], 1:2),
      column(12, intro, filters), map
    ),
    footer = column(12, footer),
    selected = "Overview",
    nav_spacer(),
    nav("Overview", page_1),
    nav("Scorecard", page_2),
    nav("Water Accounts", page_3),
    nav("My Area", page_4),
    nav("About", page_5)
  )
}

