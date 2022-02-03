#####################################################################################
# Title:   IWMI WA+ Dashboard (UI)
# Date:    Oct 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

# Footer ----
footer <- fluidRow(class="bg-dark",
  column(6, p(class="mt-4", "
    The WA+ framework is developed by the UNESCO-IHE, Delft in partnership with the
    International Water Management Institute, the Food and Agriculture Organization
    of the United Nations, and the World Water Assessment Program."),
    p(a(class="text-white", "Terms of use",
      href="https://www.iwmi.cgiar.org/about/legal-information/"), br(),
      HTML("&copy; IWMI"),
      paste(year(Sys.Date()), "All rights reserved.", sep=". "),
      br(), "Version",
      as.character(packageVersion("WADashboard")[1]),
      "(", a(class="text-white",
        href="https://mbacou.github.io/WADashboard/news/", "what's new"), ")"
    )
  ),
  column(5, offset=1,
    p(class="my-3 mr-3 text-md-right",
      a(href="https://iwmi.cgiar.org/",
        img(class="m-2", height="60px", src="./fig/iwmi_logo_w.svg")),
      a(href="https://cgiar.org/",
        img(class="m-2", height="60px", src="./fig/cgiar_w.png")),
      br(),
      a(href="https://fao.org/",
        img(class="m-2", height="50px", src="./fig/fao_logo_w.svg")),
      a(href="https://en.unesco.org/wwap/",
        img(class="m-2", height="50px", src="./fig/wwap_w.png"))
    )
  )
)

# Filters ----
filters <- fluidRow(class="pt-5 align-items-end waved3",
  column(8,
    h3(class="text-primary", "Water Accounting+ for", br(),
      em("better"), "water resource management."),
    p(class="text-muted", "Results from the Water Accounting+
        method based on global-scale public-domain datasets. WA+ objective is to
        achieve equitable and transparent water governance for all water consumers
        and to ensure a sustainable water balance.")
  ),
  column(5,
    pickerInput("txtISO3",
      span(class="text-info", "Select a river basin"),
      choices=names(ISO3), selected=init$iso3, width="16rem",
      options=pickerOptions(style="btn-white"),
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
    sliderTextInput("txtDate", NULL,
      data[iso3==init$iso3 & sheet=="sheet1"
      ][order(date_end), format(unique(date_end), "%Y %b")],
      selected=format(init$date, "%Y %b"),
      width="99%", grid=TRUE, hide_min_max=TRUE)
  )
)

# Map ----
map <- fluidRow(id="divMap", class="w-100 no-gutters collapse show",
  column(8, style="height:21rem;",
    leafletOutput("map", width="100%", height="100%")
  ),
  column(4, class="waved2",
    navs_pill(
      nav(title="Layers", icon=icon("layer-group"),
        fluidRow(class="no-gutters",
          column(12, class="pt-2",
            style="height:18.9rem; overflow:auto;",
            accordion(id="accLayers",
              accordionItem(
                title="Basin Features",
                icon=icon("caret-right"),
                class="border-0", bg="white", collapsed=FALSE,
                checkboxGroupInput("chkLayer_1", NULL, width="100%",
                  choices=names(LAYERS[[2]]$layers[3:5]))
              ),
              accordionItem(
                title="Land Cover",
                icon=icon("caret-right"),
                class="border-0", bg="white", collapsed=TRUE,
                checkboxGroupInput("chkLayer_2", NULL, width="100%",
                  choices=names(LAYERS[[2]]$layers[15:17]))
              ),
              accordionItem(
                title="Hydrology",
                icon=icon("caret-right"),
                class="border-0", bg="white", collapsed=TRUE,
                checkboxGroupInput("chkLayer_3", NULL, width="100%",
                  choices=names(LAYERS[[2]]$layers[6:14]))
              )
            )
          )
        )
      ),
      nav(title="Legend", icon=icon("palette"),
        column(12, class="mt-2 bg-white",
          style="height:18.4rem; overflow:auto;",
          uiOutput("uiLegend"))
      ),
      nav(title="Info", icon=icon("info-circle"),
        column(12, class="mt-2 pb-2 bg-white",
          style="height:18.4rem; overflow:auto;",
          uiOutput("uiInfo"))
      )
    )
  )
)


# Scorecards ----
tab_11 <- tagList(
  h3(class="text-primary", "Sustainability Score"),
  p("Impact of climate, infrastructure, and past management on long-term basin
    sustainability."),
  fluidRow(
    scoreBox("Adequate level reached", "7/10 years",
      icon=icon("tint"), footer="Environmental Water", width=6, color="success"),
    scoreBox("Fraction of water available downstream", "10%",
      icon=icon("faucet"), footer="Downstream Uses", width=6, color="warning")
  ),
  p(),
  uiOutput("ui_score_prod", inline=F),
  br()
)

tab_12 <- tagList(
  h3(class="text-primary", "Basin Variability"),
  p("Trends in storage changes"), br(),
  fluidRow(
    scoreBox("Average gain/loss", "+3% /year",
      icon=icon("check-double"), footer="Net Inflow", width=6, color="success"),
    scoreBox("Average gain/loss", "+5% /year",
      icon=icon("tint-slash"), footer="Depleted Water", width=6, color="danger")
  ),
  p(),
  uiOutput("ui_score_sust", inline=F),
  div(class="text-right",
    actionButton("btnScore", "Learn More", width="6rem", class="my-3")
  )
)

tab_13 <- tagList(
  h3(class="text-primary", "Key Facts"),
  div(class="table-responsive waved2", tableOutput("tb_basin"))
)


# Overview ----
overview <- fluidRow(
  column(4, tab_13),
  column(4, tab_11),
  column(4, tab_12)
)

# Timeline ----
timeline <- fluidRow(class="bg-white",
  column(8,
    h4(class="text-primary", "Recharge and Abstraction"),
    p("[Time-series and anomalies]"),
    highchartOutput("hcTimeline")
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
page_1 <- fluidRow(style="display:block",
  column(12, overview, timeline)
)

# Page 2 ----
page_2 <- fluidRow(style="min-height:40rem;",
  column(12,
    h4(class="text-primary", "Water Cycle"),
    p("[placeholder content for water flux dynamics]")
  )
)

# Page 3 ----
page_3 <- fluidRow(class="bg-white", style="display:block;",
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
page_5 <- fluidRow(
  column(12,
    includeMarkdown("./md/about.md"))
)

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
      column(12, filters), map, slider),
    footer = column(12, footer),
    selected = "Overview",
    nav_spacer(),
    nav("Overview", page_1, icon=icon("home")),
    nav("Water Cycle", page_2, icon=icon("sync")),
    nav("Water Accounts", page_3, icon=icon("th")),
    #nav("My Area", page_4, icon=icon("user-cog")),
    nav("About WA+", page_5, icon=icon("info-circle"))
  )
}
