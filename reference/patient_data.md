# Patient Data dataset

These datasets provide information about patients, infusion events, and
end of study events. Trial data has been de-identified and randomized
for general use.

## Usage

``` r
data(patient_data)
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 75
rows and 4 columns.

## Source

This dataset is for demonstration purposes only.

## Details

`patient_data` contains pre-formatted time series data related to
disease status markers and status markers that can support arrows. This
dataset is most applicable for use with
[`geom_swim_lane()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_lane.md).

## Examples

``` r
patient_data
#> # A tibble: 75 × 4
#>    pt_id disease_assessment       start_time end_time
#>    <chr> <chr>                         <dbl>    <dbl>
#>  1 01    CR/CRi + B Cell Recovery       -2.8      0  
#>  2 01    RD                              0        0.9
#>  3 01    CR/CRi + B Cell Aplasia         0.9      0  
#>  4 01    CR/CRi + B Cell Aplasia         0        1.8
#>  5 01    CR/CRi + B Cell Recovery        1.8      2  
#>  6 02    RD                             -2.8      0  
#>  7 02    CRi                             0        0.2
#>  8 03    CR/CRi + B Cell Recovery       -2.4      0  
#>  9 03    CR/CRi + B Cell Recovery        0        0.9
#> 10 03    CR/CRi + B Cell Aplasia         0.9      2.8
#> # ℹ 65 more rows
```
