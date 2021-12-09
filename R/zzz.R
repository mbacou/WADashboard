.onLoad <- function(libname, pkgname) {

  options(

    # Root data dir
    wa.data = if(Sys.getenv("WA_DATA") != "") Sys.getenv("WA_DATA") else
      if(dir.exists(system.file("csv", package="WADashboard")))
      system.file("csv", package="WADashboard") else "./",

    # Mapbox API key
    wa.mapbox = Sys.getenv("MAPBOX_KEY"),
    # Maptiler API key
    wa.maptiler = Sys.getenv("MAPTILER_KEY"),

    # Table formats
    xtable.sanitize.text.function = function(x)sapply(x, HTML),
    xtable.sanitize.colnames.function = function(x)sapply(x, HTML)
  )

}
