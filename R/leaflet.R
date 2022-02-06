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

#' Initialize Leafet map with default basemaps
#'
#' Load basin boundaries with default configuration. Note that we could use custom
#' vector tiles instead of raster basemaps with package
#' [mapboxapi](https://walker-data.com/mapboxapi/) to build more informative
#' hydrological layers.
#'
#' @param iso3 3-letter country code to center the map on (see [ISO3])
#' @param key Maptiler API key
#'
#' @import leaflet
#' @import leaflet.extras
#' @importFrom sf st_bbox
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
  tileset = LAYERS[["MAPTILER"]]
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

    addTiles(
      sprintf(tileset$url[[1]], tileset$layers[[3]], key),
      attribution=tileset$attr,
      group="Hillshade") %>%

    addProviderTiles("OpenStreetMap.HOT", group="OSM HOT") %>%

    addLayersControl(
      baseGroups = c("Default", "Hybrid", "OSM HOT", "Hillshade"),
      position = "bottomright"
    ) %>%
    addFullscreenControl(pseudoFullscreen=TRUE, position="topright") %>%
    addSearchOSM(searchOptions(
      position="topright", zoom=9, minLength=3, tooltipLimit=8)) %>%

    fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]])

}


#' Add external WMS tile layers
#'
#' @param map leaflet map to update
#' @param provider name of WMS provider (see [LAYERS])
#' @param date target date
#' @import leaflet
#' @importFrom lubridate days floor_date ceiling_date
#'
#' @return updated leaflet map
#' @rdname map_init
#' @export
#'
#' @examples
map_addWMSProvider <- function(map, provider="FAO", date=Sys.Date()) {

  date = as.Date(date)
  provider = match.arg(provider, names(LAYERS))
  provider = LAYERS[[provider]]

  map %>% clearGroup(names(provider$layers))

  for(i in seq_along(provider$layers)) {
    d = if(names(provider$layers)[i] %like% "Land") Sys.Date() else date
    map = addWMSTiles(map,
      baseUrl = sprintf("%sTIME=%s", provider$url[[1]], format(d, "%Y-%m")),
      layers = unname(provider$layers[[i]]),
      attribution = provider$attr[[1]],
      group = names(provider$layers)[i],
      options = WMSTileOptions(
        version="1.1.1", format="image/png", transparent=TRUE, opacity=.6)
    )
  }

  map %>%
    hideGroup(names(provider$layers))
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
#' @param provider name of WMS provider (see [LAYERS])
#' @param layers vector of layer names (see [LAYERS])
#' @param date selected timestamp
#'
#' @return updated leaflet map
#' @rdname map_init
#' @export
map_toggle <- function(map, provider="FAO", layers=NULL) {

  provider = match.arg(provider, names(LAYERS))
  provider = LAYERS[[provider]]

  # WASA data
  # map = leafem:::addGeoRaster(map, stars::read_stars(nc[[1]]$source),
  #   group = "WASA", opacity=.8,
  #   colorOptions = leafem::colorOptions(
  #     palette = viridisLite::inferno,
  #     breaks=seq(0, 10, 100)
  #   )
  # )

  if(!length(layers) > 0) layers = ""
  map %>%
    hideGroup(names(provider[["layers"]])) %>%
    showGroup(layers)
}
