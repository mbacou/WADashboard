require(data.table)
require(jsonlite)
require(sf)
require(terra)

# IMWI color palette (uses GIMP palette format)
tmp <- fread("./data-raw/json/palette.gpl", skip=4, header=F)
pal <- grDevices::rgb(tmp[, .(V1, V2, V3)], maxColorValue=255)
names(pal) <- tmp[, V4]

# Unique basin ISO3 codes and metadata
ISO3 <- read_json("./data-raw/json/Iso3.json")

# Basin and stream features (clipped)
ZOI <- lapply(ISO3, function(x) lapply(x[c("admin", "water")], st_read))

# WMS and Map Tile providers
LAYERS <- read_json("./data-raw/json/LAYERS.json")

# WA+ data cube
DATA <- readRDS("./data-raw/rds/data.rds")

# NetCDF time-series
# nc <- rast(file.path(getOption("wa.data"), "mli", "nc", "p_monthly.nc"))
# zoi <- vect(ZOI[[1]]$admin)
# ext(nc) <- ext(zoi)
# nc <- flip(nc[[1]], direction="vert",
#   filename=file.path(getOption("wa.data"), "mli", "nc", "p_monthly_4326.nc"),
#   overwrite=TRUE)
# nc <- list(sources(nc))


# Save built-in package datasets ----
usethis::use_data(ISO3, ZOI, LAYERS, DATA, pal,
  internal=TRUE, overwrite=TRUE)
