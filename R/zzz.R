.onLoad <- function(libname, pkgname) {

  options(

    # Bootstrap
    wa.elevation = 0,
    wa.font = "national-web-regular",

    # Root data dir
    wa.data = if(Sys.getenv("WA_DATA_ROOT") != "") Sys.getenv("WA_DATA_ROOT")
    else if(dir.exists("~/Projects/WADashboard/shared"))
      "~/Projects/WADashboard/shared" else "./",

    # Maptiler API key
    wa.maptiler = Sys.getenv("MAPTILER_KEY"),

    # Table formats
    xtable.sanitize.text.function = function(x)sapply(x, HTML),
    xtable.sanitize.colnames.function = function(x)sapply(x, HTML)
  )

}
