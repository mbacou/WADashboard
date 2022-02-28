#' Custom Bootstrap 4 theme
#'
#' A modified Bootstrap 4 theme based on IWMI branding guidelines.
#'
#' @inheritParams bslib::bs_theme
#' @param font_size base font size in pixel
#' @param palette color palette (list of Bootstrap colors) (see [pal])
#' @param elevation default element shadow depth (see [BS4
#'   shadows](https://getbootstrap.com/docs/4.1/utilities/shadows/))
#' @param rounded enable rounded elements (see [BS4
#'   borders](https://getbootstrap.com/docs/4.0/utilities/borders/))
#' @param gradients enable background gradients (see [BS4
#'   colors](https://getbootstrap.com/docs/4.0/utilities/colors/))
#'
#' @importFrom bslib bs_theme bs_add_variables
#' @return Modified Bootstrap 4 theme
#' @export
#' @examples
#' if(interactive()) bs_theme_preview(bs_themed())
#'
bs_themed <- function(
  base_font = "national-web-book",
  heading_font = "dm-serif-text",
  font_size = 15,
  palette = pal,
  elevation = 0,
  rounded = FALSE,
  gradients = FALSE
) {

  stopifnot(
    is.logical(rounded),
    is.logical(gradients)
  )

  # Alias
  pal = palette

  bs_theme(
    version = "4",
    bg = "white",
    fg = pal[["black"]],

    base_font = base_font,
    heading_font = heading_font,
    code_font = NULL,
    font_scale = font_size/16,

    primary = pal[["navy"]],
    secondary = pal[["light"]],
    success = pal[["green"]],
    info = pal[["blue"]],
    warning = pal[["orange"]],
    danger = pal[["red"]],

    # Use color palette
    black = pal[["black"]],
    white = "white",
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

    light = pal[["light"]],
    dark = pal[["black"]],

    `enable-rounded` = rounded,
    `enable-gradients` = gradients,

    `navbar-padding-x` = 0,
    `navbar-padding-y` = 0,
    `nav-link-padding-y` = ".75rem",
    `navbar-nav-link-padding-x` = ".75rem",
    `navbar-toggler-font-size` = "0.9rem",

    `h1-font-size` = "2.0rem",
    `h2-font-size` = "1.8rem",
    `h3-font-size` = "1.6rem",
    `h4-font-size` = "1.4rem",
    `h5-font-size` = "1.2rem",
    `h6-font-size` = "1.0rem",
    `small-font-size` = "87.5%",

    `display1-size` = "2.0rem",
    `display2-size` = "1.8rem",
    `display3-size` = "1.6rem",
    `display4-size` = "1.4rem",

    `line-height-base` = 20/font_size,
    `line-height-sm` = 20/font_size,
    `line-height-lg` = 20/font_size,
    `headings-font-weight` = 400,
    `min-contrast-ratio` = 2.5

  ) %>%
    bs_add_variables(
      .where =  "declarations",

      `navbar-brand-padding-y` = "0.5rem",

      `body-bg` = "lighten($light, 4%)",
      `border-color` = "$light",
      `gray-100` = "lighten($light, 4%)",
      `gray-200` = "darken($light, 4%)",
      `gray-300` = "darken($light, 8%)", # border-color
      `gray-400` = "darken($light, 16%)",
      `gray-500` = "darken($light, 24%)",
      `gray-600` = "darken($light, 32%)", # secondary
      `gray-700` = "darken($light, 40%)",
      `gray-800` = "darken($light, 48%)",
      `gray-900` = "darken($light, 56%)",
      `table-border-color` = "$light",

      `box-shadow-sm` = "0 .125rem .25rem -1px rgba($gray-300, 0.45)",
      `box-shadow` = "0 .5rem 2rem -1px rgba($gray-300, 0.50), inset 0 -1px 0 rgba($light, 1)",
      `box-shadow-lg` = "0 1rem 3rem -1px rgba($gray-300, 0.50), inset 0 -1px 0 rgba($light, 1)",

      `link-color` = "$blue",
      `nav-pills-link-active-bg` = "$primary",
      `input-focus-bg` = "$white",
      `input-disabled-bg` = "$white",

      `component-active-bg` = "$body-bg",
      `dropdown-border-color` = "$border-color",
      `dropdown-inner-border-radius` = 0,
      `dropdown-link-active-bg` = "$body-bg",
      `dropdown-link-hover-bg` = "$body-bg",
      `custom-select-bg` = "$input-bg",
      `custom-file-button-color` = "$input-color",
      `custom-file-button-bg` = "$input-bg",
      `label-margin-bottom` = ".05rem",

      `progress-bg` = "$light",
      `card-title-font-size` = "1rem",
      `card-spacer-y` = ".5rem",
      `card-bg` = "transparent",
      `card-cap-bg` = "transparent"
    )
}

