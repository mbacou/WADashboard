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
if(!interactive()) library(WADashboard) else devtools::load_all(".")

root <- getOption("wa.data")

#load(file.path(root, "data.RData"))
data <- file.path(root, "data_sheet.csv") %>% fread()
schema <- file.path(root, "sheet_1_schema.csv") %>% fread()

setnames(data, tolower(names(data)))
setnames(schema, tolower(names(schema)))
data[, year := as.Date(paste0(year, "-01-01"))
][schema, on=.(class, subclass, variable), id := i.id]

# Init
init = list(
  iso3 = "mli",
  date = as.Date("2017-12-31"),
  var = "var_inflow"
)
