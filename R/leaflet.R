mapboxDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "mapbox-gl",
      "1.13.2",
      system.file("js/mapbox-gl", package="WADashboard"),
      script = c("mapbox-gl.js", "leaflet-mapbox-gl.js"),
      stylesheet = c("mapbox-gl.css")
    )
  )
}

#' Initialize Leafet map and 3rd-party spatiotemporal layers
#'
#' Load basin boundaries with default configuration.
#'
#' @param iso3 3-letter country code to center the map on (see [ISO3])
#' @param key Maptiler API key
#'
#' @import leaflet
#' @import leaflet.extras
#' @importFrom sf st_bbox
#' @importFrom lubridate days
#' @return leaflet map widget with default layers
#' @rdname map_init
#' @export
#'
#' @examples
#' # Default rendering
#' map_init()
#'
map_init <- function(
  iso3 = names(ISO3),
  key = getOption("wa.maptiler")
) {

  iso3 = match.arg(iso3)
  date = Sys.Date()
  tileset = LAYERS[["MAPTILER"]]
  fao = LAYERS[["FAO"]]
  bbox = st_bbox(ZOI[[iso3]]$admin)

  m = leaflet() %>%

    # Basemaps
    addTiles(
      sprintf(tileset$url[[1]], tileset$layers[[1]], key),
      attribution=tileset$attr,
      group="Default") %>%

    addTiles(
      sprintf(tileset$url[[1]], tileset$layers[[2]], key),
      attribution=tileset$attr,
      group="Hybrid") %>%

    addProviderTiles("OpenStreetMap.HOT", group="OSM HOT") %>%
    addProviderTiles("Esri.WorldShadedRelief", group="ESRI shaded relief")

  # FAO WaPOR
  for(i in seq_along(fao$layers))
    m = addWMSTiles(m, fao$url[[1]],
      layers = unname(fao$layers[[i]]),
      attribution = fao$attr[[1]],
      group = names(fao$layers)[i],
      options = WMSTileOptions(
        version="1.1.1", format="image/png", transparent=TRUE, opacity=.6,
        time=sprintf("%s/%s", floor_date(date, "month"), ceiling_date(date, "month")-days(1))
      )
    )

  m %>%
    hideGroup(names(fao$layers)) %>%
    addLayersControl(
      baseGroups = c("Default", "Hybrid", "OSM HOT", "ESRI shaded relief"),
      position = "bottomright"
    ) %>%
    addFullscreenControl(pseudoFullscreen=TRUE, position="topright") %>%
    addSearchOSM(searchOptions(
      position="topright", zoom=9, minLength=3, tooltipLimit=8)) %>%

    fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])

}


#' Update map
#'
#' Add selected hydrological features across river basins.
#'
#' @param map leaflet map
#' @param iso3 3-letter country code to update the map (see [ISO3])
#'
#' @return leaflet map widget with default layers
#' @rdname map_init
#' @export
map_update <- function(map, iso3=names(ISO3)) {

  iso3 = match.arg(iso3)
  zoi = ZOI[[iso3]]
  bbox = st_bbox(zoi[["admin"]])

  map %>%
    # Admin boundaries
    addPolygons(data=zoi[["admin"]], group="Boundaries",
      color=pal[["orange"]], opacity=.8, weight=1,
      fill=~colorNumeric(unname(pal[1:3]), range(ADM2_CODE))(ADM2_CODE),
      fillOpacity=.1, highlightOptions=highlightOptions(fillOpacity=.8),
      label=~paste(ADM1_NAME, ADM2_NAME, sep=("\n"))
    ) %>%

    # Water streams
    addPolylines(data=zoi[["water"]], group="River Basin",
      color=pal[["blue"]], opacity=.7, smoothFactor=3,
      weight=~rescale(Class_hydr, to=c(.5, 4.5))
    ) %>%
    flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])
}


#' Toggle map layers
#'
#' Used to toggle 3rd-party contextual layers.
#'
#' @param map leaflet map
#' @param layers vector of layer names (see [LAYERS])
#'
#' @return updated leaflet map
#' @rdname map_init
#' @export
map_toggle <- function(map, layers=NULL) {

  fao = LAYERS[["FAO"]]

  # WASA data
  # map = leafem:::addGeoRaster(map, stars::read_stars(nc[[1]]$source),
  #   group = "WASA", opacity=.8,
  #   colorOptions = leafem::colorOptions(
  #     palette = viridisLite::inferno,
  #     breaks=seq(0, 10, 100)
  #   )
  # )

  if(is.null(layers) || is.na(layers)) layers = ""
  map %>%
    hideGroup(names(fao$layers)) %>%
    showGroup(layers)
}
