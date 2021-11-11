require(fresh)

load("./R/sysdata.rda")

# BS4 Theme ----
iwmi_theme <- create_theme(
  theme = "default",
  output_file = "./inst/app/www/bs4Dash.css",
  include_assets = FALSE,

  bs4dash_font(
    size_base = "0.9375rem",
    family_sans_serif = "'national-web-regular', 'sans-serif'"
  ),

  bs4dash_status(
    primary = pal[["navy"]],
    secondary = pal[["light"]],
    success = pal[["green"]],
    info = pal[["blue"]],
    warning = pal[["orange"]],
    danger = pal[["red"]],
    light = pal[["light"]], # light accent
    dark = pal[["black"]]
  ),

  bs4dash_color(
    black = "$dark",
    white = "#ffffff",
    gray_x_light = "lighten($light, 4%)",
    gray_600 = "darken($light, 8%)", # secondary status
    gray_800 = "lighten($dark, 8%)", # dark skin
    gray_900 = "$dark", # main font color

    # Use colors
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
    red = pal[["red"]]
  ),

  bs4dash_layout(
    main_bg = "lighten($light, 4%)",
    content_padding_x = 0,
    content_padding_y = 0
  ),

  bs4dash_sidebar_light(
    bg = "$light",
    color = "lighten($dark, 8%)",
    header_color = "lighten($dark, 16%)"
  ),

  bs4dash_vars(
    `gray-dark` = "lighten($dark, 40%)", # status color
    `gray` = "$main_bg", # main_bg status color
    `gray-100` = "lighten($dark, 64%)",
    `gray-200` = "lighten($dark, 56%)",
    `gray-300` = "lighten($dark, 48%)",
    `gray-400` = "lighten($dark, 40%)",
    `gray-500` = "lighten($dark, 32%)",
    `gray-700` = "lighten($dark, 16%)",

    `h1-font-size` = "1.8rem",
    `h2-font-size` = "1.6rem",
    `h3-font-size` = "1.4rem",
    `h4-font-size` = "1.2rem",
    `h5-font-size` = "1rem",
    `h6-font-size` = ".9rem",
    `headings-font-weight` = 300,
    `text-muted` = "darken($light, 50%)",
    `small-font-size` = "100%",
    `line-height-base` = 1.3,
    `line-height-lg` = 1.3,
    `line-height-sm` = 1.3,

    `enable-gradients` = TRUE,
    `border-radius` = 0,
    `border-radius-sm` = 0,
    `border-radius-lg` = 0,
    `input-color` = "$gray-800",
    `input-bg` = "$white",
    `input-disabled-bg` = "lighten($light, 4%)", # main_bg
    `input-focus-bg` = "$white",
    `input-border-color` = "$gray-400",
    `input-focus-border-color` = "$primary",
    `input-btn-padding-y` = ".2rem",

    `button-default-color` = "$gray-700",
    `button-default-border-color` = "$input-border-color",
    `button-default-background-color` = "$light",
    `button-border-radius-xs` = 0,

    `custom-select-bg` = "$input-bg",
    `custom-control-gutter` = 0,
    `dropdown-bg` = "$input-bg",
    `dropdown-border-color` = "$input-border-color",
    `custom-file-button-color` = "$input-color",
    `custom-file-button-bg` = "$input-bg",

    `component-active-color` = "$dark",
    `component-active-bg` = "transparent",

    `attachment-border-radius` = 0,
    `progress-bar-xs-border-radius` = 0,
    `progress-bar-sm-border-radius` = 0,
    `progress-bar-border-radius` = 0,
    `badge-border-radius` = ".2rem",
    `badge-pill-border-radius` = ".4rem",

    `main-footer-bg` = "$dark",
    `card-title-font-size` = "1rem",
    `card-spacer-y` = ".55rem",
    `card-bg` = "transparent",
    `link_color` = "$blue",
    `headings-font-family` = "'DM Serif Text'",
    `headings-font-weight` = 400
  )
)
