# Retrieve FontAwesome Unicode

Convert FontAwesome alias strings to their corresponding Unicode format.

All `aliases` should be prepended with "fa".

## Usage

``` r
fontawesome(aliases, type = "solid")
```

## Arguments

- aliases:

  A string or vector of strings to retrieve Unicode values for.

- type:

  A character string denoting which FontAwesome library subset to
  search. One of "solid", "regular", or "brands". Default is "solid".

## Value

A named character vector of Unicode values corresponding to the provided
aliases. If an alias is not found, its value will be `NA`.

## Details

When using fontawesome outputs with
[geom_swim_marker](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md),
the following options are available for the `family` argument:

- "FontAwesome-Brands"

- "FontAwesome-Regular"

- "FontAwesome-Solid"

## Examples

``` r
fontawesome(c("fa-car", "fa-user"))
#>  fa-car fa-user 
#>     ""     "" 
```
