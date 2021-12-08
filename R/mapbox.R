#' Initialize map (Mapbox GL)
#'
#' Load basin boundaries with default configuration.
#'
#' @param iso3 3-letter country code to center the map on (see [ISO3])
#' @param mapbox_key Mapbox API key (default to `getOption("wa.mapbox")`)
#' @param maptiler_key Maptiler API key (default to `getOption("wa.maptiler")`)
#'
#' @importFrom sf st_bbox st_as_sf
#' @return Mapbox GL map widget with default layers
#' @export
#'
#' @examples
#' # Default rendering
#' map_init()
#'
lmap_init <- function(
  iso3 = names(ISO3),
  mapbox_key = getOption("wa.mapbox"),
  maptiler_key = getOption("wa.maptiler")
) {

  iso3 = match.arg(iso3)
  tileset = LAYERS[["MAPTILER-V"]]
  fao_bmap = LAYERS[["FAO-BASEMAP"]]
  fao_data = LAYERS[["FAO-DATA"]]

  bbox = st_bbox(ZOI[[iso3]]$admin)
  coords = c(mean(bbox[1:2]), mean(bbox[3:4]))
  data = as.data.frame(rast(nc[[1]]$source), xy=TRUE, na.rm=TRUE)

  m <- mapdeck(
    token = mapbox_key,
    location = coords,
    zoom = 3,
    style = sprintf(paste0("https:", tileset$url[[1]]), tileset$layers[[1]], maptiler_key)
  ) %>%
    add_sf(ZOI[[iso3]]$admin,
      auto_highlight=TRUE,
      stroke_colour=pal[["orange"]], stroke_opacity=.8,
      fill_colour=pal[["orange"]], fill_opacity=.2)
    add_hexagon(data[1:1000,], lon="x", lat="y", colour="Band1",
      radius=111139*(data[2, "x"]-data[1, "x"]),
      layer_id="precipitation")
}


#' Update map (Mapbox GL)
#'
#' Add selected hydrological features across river basins.
#'
#' @param m leaflet map
#' @param iso3 3-letter country code to update the map (see [ISO3])
#'
#' @return Mapbox map widget
#' @export
#'
#' @examples
lmap_update <- function(m, iso3=names(ISO3)) {

  iso3 = match.arg(iso3)
  zoi = ZOI[[iso3]]
  bbox = st_bbox(zoi[["admin"]])

  admin = mapbox_source(
    type = ,

  )



  m %>%



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
