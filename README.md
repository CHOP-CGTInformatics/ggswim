
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

## Simple Data Example

Let’s look at how to pass a simple dataframe to `ggswim()`, with some
minor preprocessing to get it into an acceptable format:

``` r
library(ggswim)

my_simple_data <-
  tibble::tibble(
    id = 1:4,
    trt = rep_len(c("Drug A", "Drug B"), length.out = 4),
    time_to_last_followup = rexp(n = 4, rate = 0.5),
    time_to_death = ifelse(id %% 2, time_to_last_followup, NA),
    time_to_trt_stop = time_to_last_followup / 2
  )

my_simple_data <- my_simple_data |>
  dplyr::mutate(time_start = 0) |>
  tidyr::pivot_longer(cols = c(time_start, time_to_last_followup),
                      values_to = "time",
                      names_to = "treatment_group")
```

We’ll also define a start time and pivot the data into an easier to work
with format. We’ll also define some “markers” to signify events of
interest in the subject timelines:

``` r
my_simple_data |>
  ggswim(id = id,
         time = time,
         lane = trt) |>
  add_marker(id = id,
             name = "Death",
             time = time_to_death,
             shape = 8, size = 3, color = "firebrick", stroke = 3) |>  # ggplot args for ...
  add_marker(id = id,
             name = "Treatment 1",
             time = time_to_trt_stop,
             shape = 4, size = 3, color = "steelblue", stroke = 3)
#> Warning: Removed 4 rows containing missing values (`geom_point()`).
```

<img src="man/figures/README-ggswim simple-1.png" width="100%" />

One fun thing to note about `add_markers()` is any number of the various
aesthetic choices can be passed separate of the marker calls themselves!
In the absence of declared arguments, defaults are passed instead:

``` r
ggswim(data = my_simple_data,
       id = id,
       time = time,
       lane = trt) |>
  add_marker(id = id,
             name = "Death",
             time = time_to_death) |>  # ggplot args for ...
  add_marker(id = id,
             name = "Treatment 1",
             time = time_to_trt_stop, color = "firebrick") |>
  add_marker(id = id,
             name = "Treatment 2",
             time = time_to_trt_stop, shape = 7, size = 7) |>
  add_marker(id = id,
             name = "Treatment 3",
             time = time_to_trt_stop, stroke = 1, size = 3, color = "chartreuse2")
#> Warning: Removed 4 rows containing missing values (`geom_point()`).
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Now let’s consider a more complex set of data, where we receive our
“swim” data separate from our varying “marker” data:

``` r
my_complex_data <-
  tibble::tibble(
    subid = c(1, 1, 2, 2),
    trt = c("On-Treatment", "Off-Treatment") |> rep_len(length.out = 4),
    inttime = c(1, 4, 1, 3)
  )
my_pain_data <-
  tibble::tibble(
    subid = 1,
    time_to_pain = c(0.5, 1.5, 2.5)
  )

my_score_data <-
  tibble::tibble(
    subid = c(1,1,2,2),
    time_to_score = c(1, .75, 2.5, 0.5)
  )
```

Let’s observe how this looks via the same swim and marker functions:

``` r
complex_swim <- my_complex_data |>
  ggswim(id = subid,
         time = inttime,
         lane = trt) |> # Need to correct this
  add_marker(data = my_pain_data,
             id = subid,
             time = time_to_pain,
             name = "Pain",
             shape = 5, size = 3, color = "orange", stroke = 3) |>
  add_marker(data = my_score_data,
             id = subid,
             time = time_to_score,
             name = "Score",
             shape = 2, size = 3, color = "forestgreen", stroke = 3)

complex_swim
```

<img src="man/figures/README-ggswim complex-1.png" width="100%" />

The labels and theme may not be the most appealing though… Fortunately
`ggswim()` and `add_marker()` work directly with expected `ggplot2`
functions! Let’s give this a makeover:

``` r
complex_swim +
  ggplot2::scale_color_manual(name = "Treatment Group",
                              values = c("On-Treatment" = "skyblue", 
                                         "Off-Treatment" = "wheat2")) +
  ggplot2::scale_fill_manual(name = "Event", values = c(1,2)) +
  ggplot2::theme_minimal() +
  ggplot2::xlab("Day of Treatment") + ggplot2::ylab("Subject ID") +
  ggplot2::ggtitle("My Swim Plot")
```

<img src="man/figures/README-ggswim complex style update-1.png" width="100%" />
