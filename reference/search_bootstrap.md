# Search for Bootstrap aliases to include in ggswim

Check strings against the available aliases for Bootstrap icons.

## Usage

``` r
search_bootstrap(str = "", approximate = FALSE)
```

## Arguments

- str:

  A character string alias to search the icon data available. If left
  empty, the default "", returns all available aliases.

- approximate:

  Use approximate or exact matching, TRUE/FALSE. Default `FALSE`.

## Value

Matching aliases from the available Bootstrap data

## Examples

``` r
search_bootstrap("bs-car-front")
#> [1] "bs-car-front"      "bs-car-front-fill"
```
