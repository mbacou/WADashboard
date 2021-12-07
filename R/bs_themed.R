#' Custom Bootstrap theme
#'
#' @inheritParams bslib::bs_theme
#' @param font_size base font size in pixel
#'
#' @importFrom bslib bs_theme bs_add_variables
#' @return Modified Bootstrap 4 theme
#' @export
#' @examples
#' if (interactive()) bs_theme_preview(bs_themed())
#'
bs_themed <- function(
  base_font = getOption("wa.font")[1],
  heading_font = getOption("wa.font")[2],
  font_size = 15
) {

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

    `light` = pal[["light"]],
    `dark` = pal[["black"]],

    `enable-rounded` = FALSE,
    `enable-gradients` = FALSE,

    `navbar-padding-x` = 0,
    `navbar-padding-y` = 0,
    `navbar-nav-link-padding-x` = "1rem",
    `navbar-toggler-font-size` = ".9rem",

    `h1-font-size` = "1.8rem",
    `h2-font-size` = "1.6rem",
    `h3-font-size` = "1.4rem",
    `h4-font-size` = "1.2rem",
    `h5-font-size` = "1rem",
    `h6-font-size` = ".9rem",
    `small-font-size` = "87.5%",

    `line-height-base` = 20/font_size,
    `line-height-sm` = 20/font_size,
    `line-height-lg` = 20/font_size,
    `headings-font-weight` = 300,
    `min-contrast-ratio` = 2.5

  ) %>%

    bs_add_variables(
      .where = "declarations",

      `navbar-brand-height` = "$nav-link-height",

      `body-bg` = "lighten($light, 4%)",
      `border-color` = "darken($light, 8%)",
      `gray-100` = "lighten($light, 4%)",
      `gray-200` = "darken($light, 4%)",
      `gray-300` = "darken($light, 8%)", # border-color
      `gray-400` = "darken($light, 16%)",
      `gray-500` = "darken($light, 24%)",
      `gray-600` = "darken($light, 32%)", # secondary
      `gray-700` = "darken($light, 40%)",
      `gray-800` = "darken($light, 48%)",
      `gray-900` = "darken($light, 56%)",

      `input-focus-bg` = "$white",
      `nav-pills-link-active-bg` = "$blue",
      `input-disabled-bg` = "$white",

      `dropdown-border-color` = "$border-color",
      `dropdown-inner-border-radius` = 0,
      `component-active-bg` = "$body-bg",
      `dropdown-link-active-bg` = "$body-bg",
      `dropdown-link-hover-bg` = "$body-bg",
      `custom-select-bg` = "$input-bg",
      `custom-file-button-color` = "$input-color",
      `custom-file-button-bg` = "$input-bg",
      `label-margin-bottom` = ".25rem",

      `progress-bg` = "$light",
      `card-title-font-size` = "1rem",
      `card-spacer-y` = ".5rem",
      `card-bg` = "transparent",
      `link-color` = "$blue"
    )
}

