# Apply custom theme styling for ggplot2 plots

This function applies custom styling to various elements of ggplot2
plots, including title, subtitle, caption, axis text, axis titles, and
legend text and legend titles.

## Usage

``` r
theme_ggswim(base_size = 12, base_family = "")

theme_ggswim_dark(base_size = 12, base_family = "")
```

## Arguments

- base_size:

  The base font size to use for the plot elements. Default is 12.

- base_family:

  The base font family to use for the plot elements. Default is "".

## Value

A ggplot2 theme object.

## Details

This function builds upon the
[`theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
function in ggplot2 but overrides specific theme elements to provide a
customized look and feel for plots.

## Examples

``` r
p <- ggplot2::ggplot() +
  geom_swim_lane(
    data = patient_data,
    mapping = aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      colour = disease_assessment
    ),
    linewidth = 3
  )

p +
  theme_ggswim()


p +
  theme_ggswim_dark()
```
