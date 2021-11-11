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
#library(leaflet)
library(leaflet.extras)
#library(leaflet.extras2)
library(shinyWidgets)
library(bs4Dash)
library(data.table)
if(!interactive()) library(WADashboard)

root <- getOption("wa.data")
elev <- getOption("wa.elevation")
dir <- "../../data-raw"

data <- fread(file.path(dir, "./csv/data_sheet.csv"))
schema <- fread(file.path(dir, "./csv/sheet_1_schema.csv"))

# Format picker choices
l_iso3 <- lapply(names(ISO3), function(x) as.character(
  tagList(
    img(class="ml-5 pr-1 float-right", src=sprintf("./svg/%s.svg", x), height="30px"),
    span(class="text-lg", ISO3[[x]]["label"]),
    span(class="ml-3 text-warning", ISO3[[x]]["country"]))))

setnames(data, tolower(names(data)))
setnames(schema, tolower(names(schema)))
data[, year := as.Date(paste0(year, "-01-01"))
][schema, on=.(class, subclass, variable), id := i.id]

l_admin <- function(iso3) {
  l = ZOI[[iso3]]$admin$ADM2_CODE
  names(l) = ZOI[[iso3]]$admin$ADM2_NAME %>% as.character() %>% sort()
}

#load(file.path(root, "data.RData"))

init = list(
  iso3 = "mli",
  date = as.Date("2017-12-01"),
  var = "var_inflow"
)
