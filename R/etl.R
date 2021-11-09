#' Main data ETL
#'
#' Main ETL to ingest raw datasets found in `root`. Shiny application checks for new
#' input data files and executes this script if necessary. Exact location and
#' structure for input data files still to be defined.
#'
#' @param root base directory (local or remote) to scan for input data files (default:
#'   package option `getOption("wa.data")` or environment variable `WA_DATA_ROOT` if
#'   set)
#' @inheritParams saveRDS
#' @inheritDotParams saveRDS
#'
#' @return normalized data.table with variables:
#'   -
#' @import data.table
#' @import stringr
#' @export
#'
#' @examples
etl <- function(root=getOption("wa.data"), file=NULL, ...) {

  stopifnot(dir.exists(root))

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

  if(!is.missing(file)) saveRDS(data, file)
  return(data)
}
