% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/leaflet.R
\name{map_init}
\alias{map_init}
\alias{map_addWMSProvider}
\alias{map_update}
\alias{map_toggle}
\title{Initialize Leafet map with default basemaps}
\usage{
map_init(iso3 = names(ISO3), key = getOption("wa.maptiler"))

map_addWMSProvider(map, provider = "FAO", date = Sys.Date())

map_update(map, iso3 = names(ISO3))

map_toggle(map, provider = "FAO", layers = NULL)
}
\arguments{
\item{iso3}{3-letter country code to update the map (see \link{ISO3})}

\item{key}{Maptiler API key}

\item{map}{leaflet map}

\item{provider}{name of WMS provider (see \link{LAYERS})}

\item{date}{selected timestamp}

\item{layers}{vector of layer names (see \link{LAYERS})}
}
\value{
leaflet map widget with default layers

updated leaflet map

leaflet map widget with default layers

updated leaflet map
}
\description{
Load basin boundaries with default configuration. Note that we could use custom
vector tiles instead of raster basemaps with package
\href{https://walker-data.com/mapboxapi/}{mapboxapi} to build more informative
hydrological layers.

Add selected hydrological features across river basins.

Used to toggle 3rd-party contextual layers.
}
\examples{
# Default rendering
map_init()

}
