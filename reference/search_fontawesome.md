# Search for FontAwesome aliases to include in ggswim

Check strings against the available aliases for FontAwesome icons.

## Usage

``` r
search_fontawesome(str = "", type = "solid", approximate = FALSE)
```

## Arguments

- str:

  A character string alias to search the icon data available. If left
  empty, the default "", returns all available aliases.

- type:

  A character string denoting which FontAwesome library to search. One
  of "solid", "regular", or "brands". Default "solid".

- approximate:

  Use approximate or exact matching, TRUE/FALSE. Default `FALSE`.

## Value

Matching aliases from the available FontAwesome data

## Examples

``` r
search_fontawesome("fa-car")
#>  [1] "fa-car"                   "fa-car-battery"          
#>  [3] "fa-car-burst"             "fa-car-on"               
#>  [5] "fa-car-rear"              "fa-car-side"             
#>  [7] "fa-car-tunnel"            "fa-caravan"              
#>  [9] "fa-caret-down"            "fa-caret-left"           
#> [11] "fa-caret-right"           "fa-caret-up"             
#> [13] "fa-carrot"                "fa-cart-arrow-down"      
#> [15] "fa-cart-flatbed"          "fa-cart-flatbed-suitcase"
#> [17] "fa-cart-plus"             "fa-cart-shopping"        
```
