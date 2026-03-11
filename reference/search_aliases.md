# Utility Function to Search Aliases Across Icon Libraries

A generic function to search for aliases within specified icon
libraries.

## Usage

``` r
search_aliases(
  str = "",
  dataset = c("FontAwesome", "Bootstrap"),
  type = "solid",
  approximate = FALSE
)
```

## Arguments

- str:

  A character string alias to search the specified icon library. If left
  empty (default ""), it returns all available aliases.

- dataset:

  A character string specifying the icon library to search. Supported
  values are "FontAwesome" and "Bootstrap". Default is "FontAwesome".

- type:

  For FontAwesome dataset only. Specifies the subset to search within.
  One of "solid", "regular", or "brands". Ignored for other datasets.
  Default is "solid".

- approximate:

  Logical. If `TRUE`, performs approximate matching using `agrep`. If
  `FALSE` (default), performs exact matching using `grep`.

## Value

A sorted character vector of matching aliases.
