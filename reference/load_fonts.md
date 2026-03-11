# Load Select Fonts

Load open source fonts from ggswim's GitHub repository to the user's
cache directory.

Font files and their associated licenses are available on the main
branch.

## Usage

``` r
load_fonts(
  fonts = c("bootstrap-icons", "fa-brands-400", "fa-regular-400", "fa-solid-900"),
  verbose = TRUE
)
```

## Arguments

- fonts:

  A character vector of font families to load. Valid options are
  `"bootstrap-icons"`, `"fa-brands-400"`, `"fa-regular-400"`, and
  `"fa-solid-900"`. The default is to load all supported fonts.

- verbose:

  Whether or not to display load output messages. Default is `TRUE`.

## Value

Invisibly returns the font registry as a tibble.

## Details

Currently the following fonts are supported:

- FontAwesome

  - Brands

  - Regular

  - Solid

- Bootstrap

Users can choose which fonts to load by providing a vector of font
family names. The default is to load all supported fonts.
