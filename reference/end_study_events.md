# End of Study Events dataset

These datasets provide information about patients, infusion events, and
end of study events. Trial data has been de-identified and randomized
for general use.

## Usage

``` r
data(end_study_events)
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 7
rows and 4 columns.

## Source

This dataset is for demonstration purposes only.

## Details

`end_study_events` contains pre-formatted time series data related to
end of study events where patients left the trial for varying reasons.
This dataset is most applicable for use with
[`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md).

## Examples

``` r
end_study_events
#> # A tibble: 7 × 4
#>   pt_id time_from_initial_infusion label                     glyph
#>   <chr>                      <dbl> <chr>                     <chr>
#> 1 01                           2   Other End Study Reason    ⚠️    
#> 2 02                           0.2 Deceased                  ❌   
#> 3 03                          11.9 Completed Study Follow-Up ✅   
#> 4 05                          14.1 Completed Study Follow-Up ✅   
#> 5 06                           4.8 Other End Study Reason    ⚠️    
#> 6 08                          11.7 Other End Study Reason    ⚠️    
#> 7 12                           9.7 Other End Study Reason    ⚠️    
```
