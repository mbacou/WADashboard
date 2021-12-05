.onLoad <- function(libname, pkgname) {

  options(

    # Bootstrap
    wa.elevation = 0,
    wa.font = c("'national-web-regular'", "'DM Serif Text'"),

    # Root data dir
    wa.data = if(Sys.getenv("WA_DATA_ROOT") != "") Sys.getenv("WA_DATA_ROOT")
    else if(dir.exists(system.file("csv", package="WADashboard")))
      system.file("csv", package="WADashboard") else "./",

    # Maptiler API key
    wa.maptiler = Sys.getenv("MAPTILER_KEY"),

    # Table formats
    xtable.sanitize.text.function = function(x)sapply(x, HTML),
    xtable.sanitize.colnames.function = function(x)sapply(x, HTML)
  )

}
