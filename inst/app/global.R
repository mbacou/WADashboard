#####################################################################################
# Title:   IWMI WA+ Dashboard (GLOBAL)
# Date:    Oct 2021
# Project: WASA Visualization
# Author:  BACOU, Melanie <mel@mbacou.comm>
#####################################################################################

library(stringr)
library(scales)
library(lubridate)
library(r2d3)
library(highcharter)
library(sf)
library(leaflet.extras)
library(shinyWidgets)
library(bslib)
library(data.table)
if(interactive()) devtools::load_all(".") else library(WADashboard)

data <- DATA
meta <- META

# Init
init = list(
  iso3 = "mli",
  date = as.Date("2017-12-31"),
  var = "inc_et"
)
