#' Default color palette
#'
#' A list of Bootstrap color codes. By default this is read in from package
#' configuration file `./data-raw/json/palette.gpl` (GIMP palette format).
#'
#' @keywords datasets
#' @name pal
#' @format A named character vector
#' @examples
#'
#' @export
"pal"

#' River basin ISO3 codes
#'
#' A named list of basin metadata. Names are unique country ISO3 codes. By default
#' this list is read in from package configuration file `./data-raw/json/ISO3.json`.
#'
#' @keywords datasets
#' @name ISO3
#' @format A named list, names are 3-letter ISO3 country codes.
#' @seealso [ZOI]
#' @examples
#' # Processed river basins
#' lapply(ISO3, `[[`, "Label")
#'
#' # Sample metadata terms currently in use for each basin
#' ISO3[[1]]
#'
#' @export
"ISO3"

#' Basin boundaries
#'
#' Simplified basin boundaries and water stream networks, read in from GeoJSON files
#' at `./data-raw/json/`.
#'
#' @keywords datasets
#' @name ZOI
#' @format A named list, names are 3-letter ISO3 country codes.
#' @seealso [ISO3]
#' @examples
#' for(i in ZOI) plot(i[[admin]])
#'
#' @export
"ZOI"

#' WMS and map tile providers
#'
#' List of external spatial layers. By default layers are read in from package
#' configuration file `./data-raw/json/LAYERS.json`.
#'
#' @keywords datasets
#' @name LAYERS
#' @format A named list, names are provider codes.
#' @export
"LAYERS"
