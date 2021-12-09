#' Format basin picklist
#'
#' @return list of html tags
#' @importFrom shiny tagList
#' @export
#'
l_iso3 <- function() {
  lapply(names(ISO3), function(x) as.character(
    tagList(
      img(class="px-2 float-right", src=sprintf("./svg/%s.svg", x), height="28rem"),
      span(class="h5", ISO3[[x]]["label"]),
      span(class="mx-3 text-warning", ISO3[[x]]["country"])
    )
  ))
}

#' Format district picklist
#'
#' @param iso3 Basin ISO3 code (see [ISO3])
#'
#' @return list of html tags
#' @export
#'
#' @examples
#' l_admin("ken")
#'
l_admin <- function(iso3=names(ISO3)) {
  iso3 = match.arg(iso3)
  e = ZOI[[iso3]]$admin[, c("ADM1_NAME", "ADM2_NAME", "ADM2_CODE")]
  e = setDT(e)[order(ADM1_NAME, ADM2_NAME)] %>% split(by="ADM1_NAME")
  e = lapply(e, function(x) {
    y = x[, ADM2_CODE]
    names(y) = x[, as.character(ADM2_NAME)]
    return(y)
  })
  return(e)
}
