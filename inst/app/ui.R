#####################################################################################
# Title:   IWMI WA+ Dashboard (UI)
# Date:    Oct 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

source("navbar.R")
source("footer.R")


card1 <- makeCard(
  "Welcome to shiny.fluent demo!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text("Use the menu on the left to explore live demos of all available components.")
  ))

card2 <- makeCard(
  "shiny.react makes it easy to use React libraries in Shiny apps.",
  div(
    Text("To make a React library convenient to use from Shiny, we need to write an R package that wraps it - for example, a shiny.fluent package for Microsoft's Fluent UI, or shiny.blueprint for Palantir's Blueprint.js."),
    Text("Communication and other issues in integrating Shiny and React are solved and standardized in shiny.react package."),
    Text("shiny.react strives to do as much as possible automatically, but there's no free lunch here, so in all cases except trivial ones you'll need to do some amount of manual work. The more work you put into a wrapper package, the less work your users will have to do while using it.")
  ))

page_1 <- tagList(
  "This is a Fluent UI app built in Shiny",
  "shiny.react + Fluent UI = shiny.fluent",
  div(card1, card2)
)

page_2 <- tagList(
  div(card2)
)



# Layout ----
router <- make_router(
  route("/", page_1),
  route("page-2", page_2)
)

layout <- div(class="grid-container",
  div(class="navbar", navbar),
  div(class="header", header),
  div(class="main", router$ui),
  div(class="footer", footer)
)

addResourcePath("shiny.router", system.file("www", package="shiny.router"))
shiny_router_js <- file.path("shiny.router", "shiny.router.js")

# UI ----
fluentPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$script(type="text/javascript", src=shiny_router_js),
    tags$script(type="text/javascript", src="theme.js")
  ),
  layout
)
