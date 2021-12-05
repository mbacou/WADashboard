require(data.table)
require(jsonlite)
require(sf)

# IMWI color palette (from GIMP palette format) ----
tmp <- fread("./data-raw/json/palette.gpl", skip=4, header=F)
pal <- grDevices::rgb(tmp[, .(V1, V2, V3)], maxColorValue=255)
names(pal) <- tmp[, V4]

# Basin ISO3 codes and metadata
ISO3 <- read_json("./data-raw/json/ISO3.json")

# Basin boundaries (clipped) ----
ZOI <- lapply(ISO3, function(x) lapply(x[c("admin", "water")], st_read))

# WMS and Map Tile providers ----
LAYERS <- read_json("./data-raw/json/LAYERS.json")

# Save package datasets ----
usethis::use_data(ISO3, ZOI, LAYERS, pal,
  internal=TRUE, overwrite=TRUE)

