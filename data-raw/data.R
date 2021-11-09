require(data.table)
require(sf)

# IMWI color palette ----
tmp <- fread("./data-raw/iwmi.gpl", skip=4, header=F)
pal <- grDevices::rgb(tmp[, .(V1, V2, V3)], maxColorValue=255)
names(pal) <- tmp[, V4]

# Basin ISO3 codes
ISO3 <- list(
  ken = c(label="Mara River", country="Kenya"),
  mli = c(label="Niger River", country="Mali")
)

# WMS and Map Tile providers ----
SOURCES <- list(

  `DEA` = list(
    url = "https://ows.digitalearth.africa/",
    attr = "Digital Earth Africa",
    layers = c("")
  ),

  `FAO-BASEMAP` = list(
    url = "https://io.apps.fao.org/geoserver/wms?",
    attr = "FAO WaPOR",
    layers = c(
      `Natural Earth` = "RICCAR:gray_hr_sr_w_3",
      `Blue Marble` = "NASA:bluemarble_ls_sw_st"
    )
  ),

  `FAO-DATA` = list(
    url = "https://io.apps.fao.org/geoserver/wms?",
    attr = "FAO WaPOR",
    layers = c(
      `Hydrological Basins` = "AQUAMAPS:hydrobasins_africa",
      `Major Rivers` = "AQUAMAPS:rivers_africa",
      `Land Cover 30m` = "WAPOR_2:l3_odn_lcc_d",
      `Monthly Precipitation 5km` = "WAPOR_2:l1_pcp_m",
      `Actual ET` = "WAPOR_2:l3_odn_aeti_d",
      `Incremental ET` = "WATER:niger_etincr",
      `Drought Intensity` = "ASIS:di_d",
      `Monthly Precipitation Anomaly` = "NMME:fmpa"
    )
  ),

  `MAPTILER` = list(
    url = "//api.maptiler.com/maps/%s/{z}/{x}/{y}@2x.png?key=%s",
    attr = "OSM | FAO WaPOR",
    key = Sys.getenv("MAPTILER_KEY"),
    layers = c(
      `default` = "42a84100-2300-4647-ba8f-a70afaf51946"
    )
  )
)

# Basin boundaries (clipped) ----
ZOI <- list(
  ken = list(
    admin = st_read("./data-raw/json/mli_basin.geojson"),
    water = st_read("./data-raw/json/mli_gloric.geojson")
  ),
  mli = list(
    admin = st_read("./data-raw/json/mli_basin.geojson"),
    water = st_read("./data-raw/json/mli_gloric.geojson")
  )
)

# Save package datasets ----
usethis::use_data(ISO3, ZOI, SOURCES, pal,
  internal=TRUE, overwrite=TRUE)
