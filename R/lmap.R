#' Initialize map
#'
#' Load map with default configuration.
#'
#' @param iso3 3-letter country code to center the map on (see [ISO3])
#'
#' @import leaflet
#' @import leaflet.extras
#' @return
#' @export
#'
#' @examples
lmap_init <- function(iso3=names(ISO3)) {

  iso3 = match.arg(tolower(iso3), names(ISO3))
  tileset = SOURCES[["MAPTILER"]]
  fao_bmap = SOURCES[["FAO-BASEMAP"]]
  fao_data = SOURCES[["FAO-DATA"]]

  key = tileset$key
  key = if(key != "")  key else getOption("wa.maptiler")

  bbox = st_bbox(ZOI[[1]]$admin)

  # Performance
  opts = leafletOptions(preferCanvas=TRUE)

  m = leaflet(options=opts) %>%

    # Default config
    addGraticule(1, group="Graticule",
      style=list(color=pal[["light"]], weight=.2)) %>%
    addTiles(
      sprintf(tileset$url, tileset$layers[1], key),
      attribution = tileset$attr,
      group = "Default")

  # WaPOR basemaps
  for(i in seq_along(fao_bmap$layers))
    m = addWMSTiles(m, fao_bmap$url,
      layers = unname(fao_bmap$layers[i]),
      attribution = fao_bmap$attr,
      group = names(fao_bmap$layers)[i],
      options = WMSTileOptions(
        version="1.1.1", format="image/png", transparent=TRUE)
    )

  # WaPOR data
  for(i in seq_along(fao_data$layers))
    m = addWMSTiles(m, fao_data$url,
      layers = unname(fao_data$layers[i]),
      attribution = fao_data$attr,
      group = names(fao_data$layers)[i],
      options = WMSTileOptions(
        version="1.1.1", format="image/png", transparent=TRUE)
    )

  m %>%
    addLayersControl(
      baseGroups=c("Default", names(fao_bmap$layers)),
      overlayGroups=c("Graticule", "Boundaries", "River Basin", names(fao_data$layers)),
      position="bottomright"
    ) %>%

    hideGroup(c("Graticule", names(fao_data$layers))) %>%
    addSearchOSM(searchOptions(
      position="topright", zoom=9, minLength=3, tooltipLimit=8, )) %>%
    addFullscreenControl(pseudoFullscreen=TRUE, position="topright")
}


#' Update map
#'
#' Add selected hydrological features across river basins.
#'
#' @param m leaflet map
#' @param iso3 3-letter country code to update the map (see [ISO3])
#'
#' @return
#' @export
#'
#' @examples
lmap_update <- function(m, iso3=names(ISO3)) {

  iso3 = match.arg(tolower(iso3), names(ISO3))
  zoi = ZOI[[iso3]]
  bbox = st_bbox(zoi[["admin"]]) + c(1,1,-1,-1)

  m %>%
    fitBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]]) %>%

    # Admin boundaries
    addPolygons(data=zoi[["admin"]], group="Boundaries",
      color=pal[["green"]], opacity=.9, weight=1,
      fill=~colorNumeric(unname(pal[1:3]), range(ADM2_CODE))(ADM2_CODE),
      fillOpacity=.4,
      label=~paste(ADM1_NAME, ADM2_NAME, sep="<br/>")
    ) %>%

    # Water streams
    addPolylines(data=zoi[["water"]], group="River Basin",
      color=pal[["blue"]], opacity=.9, smoothFactor=3,
      weight=~rescale(Class_hydr, to=c(1, 5))
    )
}
