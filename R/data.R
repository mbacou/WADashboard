#' Default color palette
#'
#' A list of Bootstrap color codes. By default colors are read in from package
#' configuration file at `./data-raw/json/palette.gpl` (GIMP palette format).
#'
#' @keywords datasets
#' @name pal
#' @format A named character vector
#' @examples
#'
#' @export
"pal"

#' River basins
#'
#' A named list of basin metadata. Names are the basin's largest country ISO3 codes.
#' This list is read in from package configuration file at
#' `./data-raw/json/ISO3.json`. This file may be manually edited to add new basin
#' configurations.
#'
#' @keywords datasets
#' @name ISO3
#' @format A named list, names are 3-letter ISO3 country codes.
#' @seealso [ZOI]
#' @examples
#' # Processed river basins
#' names(ISO3)
#'
#' # Metadata terms currently in use for each basin
#' ISO3[[1]]
#'
#' @export
"ISO3"

#' Basin boundaries
#'
#' Named list of simplified basin boundaries and water stream networks as `sf`
#' objects, read in from GeoJSON file locations defined in [ISO3].
#'
#' @keywords datasets
#' @name ZOI
#' @format A named list, names are 3-letter ISO3 country codes.
#' @seealso [ISO3]
#' @examples
#' par(mfrow=c(1, length(ZOI)))
#' for(i in ZOI) {
#'  plot(st_geometry(i[["admin"]]), col=pal["light"])
#'  plot(st_geometry(i[["water"]]), col=pal["blue"], add=T)
#' }
#'
#' @export
"ZOI"

#' WMS and map tile providers
#'
#' Named list of external spatial layers. By default layers are read in from package
#' configuration file at `./data-raw/json/LAYERS.json`. This file may be manually
#' edited to add new layers to the dashboard. All spatial formats supported by the
#' [Leaflet](https://leafletjs.com/) JS viewer are supported (e.g. raster tiles, WMS,
#' GeoJSON, KML, rasters, etc.), aside from **vector tiles**.
#'
#' The list is organized by data provider (e.g. Digital Earth Africa, ESRI World
#' Atlas, FAO WaPOR, etc.).
#'
#' @keywords datasets
#' @name LAYERS
#' @format A named list, names are provider codes.
#' @examples
#' LAYERS[[2]]
#'
#' @export
"LAYERS"
