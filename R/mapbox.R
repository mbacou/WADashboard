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
  tileset = LAYERS[["MAPTILERV"]]
  fao = LAYERS[["FAO"]]

  bbox = st_bbox(ZOI[[iso3]]$admin)
  coords = c(mean(bbox[1:2]), mean(bbox[3:4]))
  data = as.data.frame(rast(nc[[1]]$source), xy=TRUE, na.rm=TRUE)

  m <- mapdeck(
    token = mapbox_key,
    location = coords,
    zoom = 3,
    style = sprintf(paste0("https:", tileset$url[[1]]), tileset$layers[[1]], maptiler_key)
  ) %>%
    add_terrain(
      data="https://api.maptiler.com/tiles/terrain-quantized-mesh/{z}/{x}/{y}.quantized-mesh-1.0?key=YM9A7w168nwbyjivlfzB"
      ) %>%
    add_polygon(ZOI[[iso3]]$admin,
      auto_highlight=TRUE, highlight_colour=alpha(pal[["orange"]], .5),
      stroke_colour=alpha(pal[["orange"]], .9), fill_colour=alpha(pal[["orange"]], .4)) %>%
    add_sf(ZOI[[iso3]]$water,
      stroke_width="Class_hydr", stroke_colour=alpha(pal[["blue"]], .9))
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
