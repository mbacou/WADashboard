#####################################################################################
# Title:   IWMI WA+ Dashboard (UI)
# Date:    Oct 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

# Footer ----
footer <- fluidRow(class="bg-dark align-items-center",
  column(8,
    fluidRow(
      div(class="mx-3 pt-3 pb-1",
        a(href="https://iwmi.cgiar.org/",
          img(height="50px", src="./fig/iwmi_logo_w.svg"))
      ),
      div(class="mx-3 pt-2 pb-2",
        a(href="https://cgiar.org/",
          img(height="60px", src="./fig/cgiar_w.png"))
      ),
      div(class="mx-3 pt-3",
        a(href="https://fao.org/",
          img(height="50px", src="./fig/fao_logo_w.svg"))
      ),
      div(class="mx-3 pt-3",
        a(href="https://en.unesco.org/wwap/",
          img(height="50px", src="./fig/wwap_w.png"))
      )
    )
  ),
  column(4, class="pt-3 text-md-right",
    p(a(class="text-white", "Terms of use",
      href="https://www.iwmi.cgiar.org/about/legal-information/"), br(),
      HTML("&copy; IWMI"),
      paste(year(Sys.Date()), "All rights reserved", sep=". "),
      br(), "Version",
      as.character(packageVersion("WADashboard")[1]),
      "(", a(class="text-white",
        href="https://github.com/mbacou/WADashboard", "what's new"), ")"
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
filters <- fluidRow(class="mt-3 pt-5 align-items-end waved3",
  column(9,
    p(class="text-muted",
      "This dashboard compiles results from the",
      a(class="text-gray-dark", href="https://wateraccounting.org/", target="wa",
        "Water Accounting+"),
      "method based on global-scale public-domain datasets. Its
      objective is to achieve equitable and transparent water governance for
      all water consumers and to ensure a sustainable water balance.")
  ),
  column(5,
    pickerInput("txtISO3",
      span(class="text-info", "Select a river basin"),
      choices=names(ISO3), selected=init$iso3, width="16rem",
      options=pickerOptions(style="btn-white btn-lg"),
      choicesOpt=list(content=l_iso3()))
  ),
  column(7,
    fluidRow(class="no-gutters float-md-right align-items-end",
      div(class="col pb-3",
        tags$label(class="text-info", "Last model run"), br(),
        actionButton("btnRefresh",
          span(strong(format(init$date, "%Y %b"))),
          icon=icon("sync"), class="btn-outline-info btn-sm", width="9rem")
      ),
      div(class="col pl-2",
        radioGroupButtons("txtUnit",
          span(class="text-info", "Flow units"), c("km³", "ft³", "MCM"),
          status="outline-info", justified=TRUE, size="sm", width="9rem")
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
slider <- fluidRow(class="w-100 no-gutters",
  column(12, class="waved border-bottom border-top px-4 pt-2 pb-0",
    div(class="float-left", p("Basin Timeline")),
    sliderTextInput("numYear", NULL,
      data[iso3==init$iso3 & sheet=="sheet1"][order(year), format(unique(year), "%Y %b")],
      selected=data[, format(max(year), "%Y %b")],
      width="98%", grid=TRUE, hide_min_max=TRUE)
  )
)

# Map ----
map <- fluidRow(id="divMap", class="w-100 no-gutters collapse show",
  column(8, style="min-height:20rem; height:34vh;",
    leafletOutput("map", width="100%", height="100%")
  ),
  column(4, class="waved3 text-muted",
    navs_pill(
      nav(title="Layers", icon=icon("layer-group"),
        br(),
        checkboxGroupInput("chkLayer", NULL, width="100%",
          choices=names(LAYERS[[3]]$layers))
      ),
      nav(title="Legend", icon=icon("palette"),
        h4("Layers 2"),
        p("[placeholder]"),
        p("Map layer options")
      ),
      nav(title="Info", icon=icon("info-circle"),
        h4("Layers 2"),
        p("[placeholder]"),
        p("Map layer options")
      )
    )
  )
)

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
    p("[basin timeline and anomalies]"),
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
page_2 <- fluidRow(style="min-height:40rem;",
  column(12,
    h4(class="text-info", "Water Cycle"),
    p("[placeholder content for water flux dynamics]")
  )
)

# Page 3 ----
page_3 <- fluidRow(class="bg-white", style="display:block;",
  navs_bar(
    title = span(class="h4 text-info", "Water Accounts"),
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
    h4(class="text-info", "My Area"),
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
page_5 <- fluidRow(style="min-height:30rem",
  column(12,
    h4(class="text-info", "About WA+"),
    p("Units"),
    p("Land use categories"),
    p("Model structure"),
    p("Data Catalog")
  )
)

function() {
  page_navbar(
    id = "navPage",
    theme = bs_themed(),
    window_title = "IWMI | Water Accounting+",
    title = tagList(
      span(class="h3 text-primary", "WATER Accounting+"),
      span(class="mx-4 text-warning", "DRAFT")
    ),
    bg = alpha("white", .9),
    position = "fixed-top",
    fluid = TRUE,
    header = tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="iwmi.css"),
        tags$link(rel="shortcut icon", href="favicon.ico")
      ),
      column(12, filters), map, slider),
    footer = column(12, footer),
    selected = "Overview",
    nav_spacer(),
    nav("Overview", page_1),
    nav("Water Cycle", page_2),
    nav("Water Accounts", page_3),
    nav("My Area", page_4),
    nav("About WA+", page_5)
  )
}
