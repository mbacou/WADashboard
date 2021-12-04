#' Custom Bootstrap theme
#'
#' @return
#' @import bslib
#' @importFrom scales alpha
#' @export
#'
#' @examples
bs_themed <- function() {
  bs_theme(
    version = "4",
    bg = "white",
    fg = pal[["black"]],

    primary = pal[["navy"]],
    secondary = pal[["light"]],
    success = pal[["green"]],
    info = pal[["blue"]],
    warning = pal[["orange"]],
    danger = pal[["red"]],

    base_font = "'national-web-regular'",
    code_font = NULL,
    heading_font = "'DM Serif Text'",
    font_scale = 0.9375,

    light = pal[["light"]],
    dark = pal[["black"]],

    # Use colors
    black = pal[["black"]],
    white = "#ffffff",
    yellow = pal[["yellow"]],
    orange = pal[["orange"]],
    lime = pal[["light-blue"]],
    olive = pal[["teal"]],
    green = pal[["green"]],
    teal = pal[["teal"]],
    cyan = pal[["teal"]],
    lightblue = pal[["light-blue"]],
    blue = pal[["blue"]],
    navy = pal[["navy"]],
    fuchsia = pal[["purple"]],
    purple = pal[["purple"]],
    maroon = pal[["maroon"]],
    red = pal[["red"]],

    `enable-rounded` = FALSE,
    `enable-gradients` = FALSE,

    `navbar-padding-x` = 0,
    `navbar-padding-y` = 0,
    `navbar-nav-link-padding-x` = "1rem",
    `navbar-toggler-font-size` = ".9rem",
    `nav-link-padding-x` = "1rem",
    `input-focus-width` = 0,
    `input-border-width` = 0

  ) %>%

    bs_add_variables(
      .where = "declarations",

      `navbar-brand-height` = "$nav-link-height",

      `body-bg` = "lighten($light, 4%)",
      `gray-100` = "lighten($light, 4%)",
      `gray-200` = "darken($light, 4%)",
      `gray-300` = "darken($light, 8%)",
      `gray-400` = "darken($light, 16%)",
      `gray-500` = "darken($light, 24%)",
      `gray-700` = "darken($light, 32%)",
      `gray-800` = "darken($light, 40%)",
      `gray-900` = "darken($light, 48%)",

      `h1-font-size` = "1.8rem",
      `h2-font-size` = "1.6rem",
      `h3-font-size` = "1.4rem",
      `h4-font-size` = "1.2rem",
      `h5-font-size` = "1rem",
      `h6-font-size` = ".9rem",
      `headings-font-weight` = 300,
      `line-height-base` = 20/15,

      `input-focus-bg` = "$white",
      `nav-tabs-link-active-bg` = "$white",
      `input-disabled-bg` = "$white",

      `custom-select-bg` = "$input-bg",
      `dropdown-bg` = "$input-bg",
      `dropdown-border-color` = "$input-border-color",
      `custom-file-button-color` = "$input-color",
      `custom-file-button-bg` = "$input-bg",
      `label-margin-bottom` = ".25rem",

     # `component-active-bg` = "lighten($light, 2%)",
      `custom-select-indicator-color` = "$light",
      `progress-bg` = "$light",

      `card-title-font-size` = "1rem",
      `card-spacer-y` = ".5rem",
      `card-bg` = "$white",
      `link-color` = "$blue",
      `headings-font-family` = "'DM Serif Text'",
      `headings-font-weight` = 400,
      `min-contrast-ratio` = 2.5

    )
}

