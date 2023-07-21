
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggswim

<p align="center">

<img src="man/figures/ggswim.png" alt="ggswim hex logo" width="150" align="right"/>

</p>
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/CHOP-CGTInformatics/ggswim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CHOP-CGTInformatics/ggswim/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The ggswim package provides an easy set of commands to create “swimmer”
plots.

## Installation

You can install the development version of ggswim like so:

``` r
devtools::install_github("CHOP-CGTInformatics/ggswim")
```

## Example

Below is a simple example for how to set up your “swim table” to pass
along to `ggswim()`:

``` r
library(ggswim)

df <- tibble::tribble(
  ~subject_id, ~time, ~event,
  1, 0, "Infusion",
  1, 2, "CRS Grade 1",
  1, 3, "CRS Grade 2",
  1, 4, "CRS Grade 1",
  1, 5, "No CRS",
  1, 6, "Last Follow Up",
  2, 0, "Infusion",
  2, 1, "CRS Grade 1",
  2, 4, "CRS Grade 2",
  2, 7, "CRS Grade 1",
  2, 8, "No CRS",
  2, 9, "Last Follow Up",
  3, 0, "Infusion",
  3, 1, "CRS Grade 1",
  3, 2, "CRS Grade 2",
  3, 3, "CRS Grade 4",
  3, 7, "CRS Grade 5",
  3, 10, "Death"
)

df
```

We’ll also pre-define some arguments of interest and the plot
accordingly. To showcase the different ways you can supply `markers` and
`lanes` to `ggswim()`, we’ll show a few examples together:

``` r
lanes = list(
  "No CRS" = "gray",
  "CRS Grade 1" = "yellow",
  "CRS Grade 2" = "orange",
  "CRS Grade 3" = "pink",
  "CRS Grade 4" = "red",
  "CRS Grade 5" = "black"
)

markers_emoji <- list(
  "Last Follow Up" = "👤",
  "Death" = "❌",
  "Infusion" = "🏥"
)

plot1 <- df |>
  ggswim(id = subject_id,
         time = time,
         events = event,
         reference_event = "Infusion",
         markers = markers_emoji,
         lanes = lanes,
         legend_title = c("My Lanes Legend", "My Markers Legend")) +
  ggplot2::labs(title = "My Swim Plot with Emojis", x = "Time", y = "Subject ID")

markers_shape <- list(
  "Last Follow Up" = 21,
  "Death" = 4,
  "Infusion" = 6
)

unnamed_lanes = list(
  "No CRS",
  "CRS Grade 1",
  "CRS Grade 2",
  "CRS Grade 3",
  "CRS Grade 4",
  "CRS Grade 5"
)

plot2 <- df |>
  ggswim(id = subject_id,
         time = time,
         events = event,
         reference_event = "Infusion",
         markers = markers_shape,
         lanes = unnamed_lanes,
         legend_title = c("My Lanes Legend", "My Markers Legend")) +
  ggplot2::labs(title = "My Swim Plot with Shapes", x = "Time", y = "Subject ID")
```

The first plot (top) shows `markers` defined by emojis, with `lanes`
assigned user-defined colors. The second plot (bottom) shows `markers`
defined by [ggplot
shapes](https://r-graphics.org/recipe-scatter-shapes#discussion-28) and
`lanes` with no color designation. In this case, default ggplot colors
are assigned.

``` r
plot1 +
  ggplot2::labs(title = "My Swim Plot", x = "Time", y = "Subject ID") +
  ggplot2::theme_minimal()
```

<img src="man/figures/README-plot1-1.png" width="100%" />

``` r
plot2 +
  ggplot2::labs(title = "My Swim Plot", x = "Time", y = "Subject ID") +
  ggplot2::theme_minimal()
```

<img src="man/figures/README-plot2-1.png" width="100%" />

One key thing to note is that lanes and markers are ordered as factors
in the order they are received. Additionally, the first lane specifier
is considered the default to be used in the absence of lane data in the
event stream. Notice how “No CRS” is assigned at the beginning of the
swim lanes even though it is not defined here in the original `df`.
