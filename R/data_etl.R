#'Main data ETL
#'
#'Main ETL to ingest WA+ modeled result sets found at location defined by
#'`getOption("wa.data")`. This location may be specified at run time or via
#'environment variable `WA_DATA`. Exact location and structure for output data files
#'are still to be defined.
#'
#'A [vignette](../articles/02_data-etl.html) is available to describe the current approach.
#'
#'@param iso3 3-letter country code of the river basin to ingest. If null, all basin
#'  ISO3 listed in [ISO3] will be processed. Can be used to manually test new result
#'  sets.
#'@param root base directory (local or remote) to scan for input data files (default:
#'  package option `getOption("wa.data")` or environment variable `WA_DATA` if set)
#'@param file a connection or the name of the file where the R object is saved to. The
#'  default behavior is to save the transformed dataset to this package's `./data-raw`
#'  folder, and to include it as a built-in package dataset (this is done
#'  automatically on package build, see [DATA]).
#'
#'@return normalized data.table with variables: -
#'@import data.table
#'@import stringr
#'
#' @examples
#' data <- data_etl("mli")
#'
#'@export
data_etl <- function(
  iso3 = NULL,
  root = getOption("wa.data"),
  file = file.path("./data-raw/rds/data.rds")
) {

  stopifnot(dir.exists(root))
  iso3 = if(missing(iso3)) names(ISO3) else match.arg(names(ISO3))

  # List sources
  src = list(
    ken = "./ken/hydroloop_results/csv",
    mli = "./mli/csv_km3"
  )

  # Consolidate
  src =
    file.path(dir, data) %>%
    lapply(list.files, pattern="*.csv", recursive=TRUE, full.names=TRUE) %>%
    lapply(data.table) %>%
    rbindlist(idcol="iso3", use.names=TRUE, fill=TRUE) %>%
    setnames("V1", "path")

  # Combine all yearly and monthly output
  src[,
    file := basename(path)
  ][, `:=`(
    year = str_extract(file, "_[0-9]{4}") %>% str_sub(2,5) %>% as.integer(),
    month = str_extract(file, "[0-9]{4}_[0-9]{1,2}") %>% str_sub(6,7) %>% as.integer(),
    sheet = str_extract(tolower(file), "sheet[0-9]{1}")
  )] %>% setorder(iso3, year, month, na.last=TRUE)

  #



  data = lapply(1:nrow(src), function(x)
    fread(src[x, path])[, `:=`(
      iso3 = f[x, iso3],
      sheet = f[x, sheet],
      year = f[x, year]
    )]) %>% rbindlist()

  return(data)
}
