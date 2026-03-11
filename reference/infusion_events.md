# Infusion Events dataset

These datasets provide information about patients, infusion events, and
end of study events. Trial data has been de-identified and randomized
for general use.

## Usage

``` r
data(infusion_events)
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 18
rows and 5 columns.

## Source

This dataset is for demonstration purposes only.

## Details

`infusion_events` contains pre-formatted time series data related to
infusions and reinfusions for patients in `patient_data`. This dataset
is most applicable for use with
[`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md).

## Examples

``` r
infusion_events
#> # A tibble: 18 × 5
#>    pt_id time_from_initial_infusion label             glyph colour 
#>    <chr>                      <dbl> <chr>             <chr> <chr>  
#>  1 01                           1   First Reinfusion  ⬤     #999999
#>  2 02                           0   First Reinfusion  ⬤     #999999
#>  3 03                           2   First Reinfusion  ⬤     #999999
#>  4 03                           3   Second Reinfusion ⬤     #f57dc1
#>  5 04                           5   First Reinfusion  ⬤     #999999
#>  6 05                           2   First Reinfusion  ⬤     #999999
#>  7 05                           4.3 Second Reinfusion ⬤     #f57dc1
#>  8 06                           2   First Reinfusion  ⬤     #999999
#>  9 08                           1   First Reinfusion  ⬤     #999999
#> 10 08                           2.5 Second Reinfusion ⬤     #f57dc1
#> 11 09                           7   First Reinfusion  ⬤     #999999
#> 12 12                           6   First Reinfusion  ⬤     #999999
#> 13 13                           1   First Reinfusion  ⬤     #999999
#> 14 14                           0   First Reinfusion  ⬤     #999999
#> 15 15                           0   First Reinfusion  ⬤     #999999
#> 16 17                           1   First Reinfusion  ⬤     #999999
#> 17 18                           3   First Reinfusion  ⬤     #999999
#> 18 19                           4   First Reinfusion  ⬤     #999999
```
