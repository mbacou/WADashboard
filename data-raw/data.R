require(data.table)
require(jsonlite)
require(sf)
require(terra)

# IMWI color palette (uses GIMP palette format)
tmp <- fread("./data-raw/json/palette.gpl", skip=4, header=F)
pal <- grDevices::rgb(tmp[, .(V1, V2, V3)], maxColorValue=255)
names(pal) <- tmp[, V4]

# Unique basin ISO3 codes and metadata
ISO3 <- fromJSON("./data-raw/json/ISO3.json", flatten=TRUE)

# Basin and stream features (clipped)
ZOI <- lapply(ISO3, function(x) lapply(x[c("admin", "water")], st_read))

# WMS and Map Tile providers
LAYERS <- fromJSON("./data-raw/json/LAYERS.json", flatten=TRUE)

# WA+ data cube
DATA <- list(
  s1 = "./data-raw/rds/data_sheet_1.rds",
  s2 = "./data-raw/rds/data_sheet_2.rds"
) %>% lapply(readRDS) %>% lapply(melt, id.vars=1:7, variable.name="id") %>% rbindlist()

# WA+ metadata
META <- list(
  s1 = "./data-raw/csv/sheet_1_schema.csv",
  s2 = "./data-raw/csv/sheet_2_schema.csv"
) %>% lapply(fread) %>% rbindlist(fill=TRUE)

# NetCDF time-series
# nc <- rast(file.path(getOption("wa.data"), "mli", "nc", "p_monthly.nc"))
# zoi <- vect(ZOI[[1]]$admin)
# ext(nc) <- ext(zoi)
# nc <- flip(nc[[1]], direction="vert",
#   filename=file.path(getOption("wa.data"), "mli", "nc", "p_monthly_4326.nc"),
#   overwrite=TRUE)
# nc <- list(sources(nc))


# Save built-in package datasets ----
usethis::use_data(ISO3, ZOI, LAYERS, DATA, META, pal,
  internal=TRUE, overwrite=TRUE)
