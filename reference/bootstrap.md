# Retrieve Bootstrap Unicode

Convert Bootstrap alias strings to their corresponding Unicode format.

All `aliases` should be prepended with "bs".

## Usage

``` r
bootstrap(aliases)
```

## Arguments

- aliases:

  A string or vector of strings to retrieve Unicode values for.

## Value

A named character vector of Unicode values corresponding to the provided
aliases. If an alias is not found, its value will be `NA`.

## Details

When using bootstrap outputs with
[geom_swim_marker](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md),
the following options are available for the `family` argument:

- "Bootstrap"

## Examples

``` r
bootstrap(c("bs-car-front", "bs-heart"))
#> bs-car-front     bs-heart 
#>          ""          "" 
```
