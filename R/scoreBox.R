#' Scorecard Box
#'
#' Modified version of [bs4Dash::infoBox()]
#'
#' @inheritParams bs4Dash::valueBox
#'
#' @return
#' @importFrom bs4Dash valueBox
#' @export
#'
#' @examples
scoreBox <- function(
  value,
  subtitle,
  icon = NULL,
  color = NULL,
  width = 3,
  href = NULL,
  footer = NULL,
  gradient = FALSE,
  elevation = NULL
)  valueBox(
  value,
  subtitle,
  icon,
  color,
  width,
  href,
  footer,
  gradient,
  elevation
)
