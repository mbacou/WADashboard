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
library(shinyWidgets)
library(bslib)
if(interactive()) devtools::load_all(".") else library(WADashboard)

pkg <- system.file(package="WADashboard")
data <- DATA
schema <- file.path(pkg, "./csv/sheet_1_schema.csv") %>% fread()

setnames(schema, tolower(names(schema)))
data[schema, on=.(class, subclass, variable), id := i.id]

# Init
init = list(
  iso3 = "mli",
  date = as.Date("2017-12-31"),
  var = "var_inflow"
)
