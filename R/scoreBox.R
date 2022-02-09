#' Scorecard Box
#'
#' Modified version of [bs4Dash::valueBox()]
#'
#' @inheritParams bs4Dash::valueBox
#'
#' @return
#' @importFrom bs4Dash valueBox
#' @importFrom shiny HTML
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
  HTML('<span class="h2">', subtitle, '</span>'),
  icon,
  color,
  width,
  href,
  footer,
  gradient,
  elevation
)
