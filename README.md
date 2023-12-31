
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggswim <a href="https://chop-cgtinformatics.github.io/ggswim/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/CHOP-CGTInformatics/ggswim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CHOP-CGTInformatics/ggswim/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/CHOP-CGTInformatics/ggswim/branch/main/graph/badge.svg)](https://app.codecov.io/gh/CHOP-CGTInformatics/ggswim?branch=main)
<!-- badges: end -->

The ggswim package provides a convenient set of commands to easily
create “swimmer” plots.

## Installation

You can install the development version of ggswim like so:

``` r
devtools::install_github("CHOP-CGTInformatics/ggswim")
```

## ggswim: Elegant Swimmer Plots with ggplot2

ggswim simplifies the creation of stunning swimmer plots by seamlessly
integrating with the familiar ggplot2 framework, allowing developers to
effortlessly add layers and customize displays to their needs.

One of ggswim’s key strengths is its ability to streamline the process
of generating clear, reproducible legends. This ensures that crucial
points of interest are effectively communicated, enhancing the narrative
of the swimmer plot.

Let’s dive right into a quick example to showcase the simplicity and
effectiveness of ggswim!

## Exploring the Sample Dataset

To help you get started, ggswim includes a sample dataset named
`patient_status`. This dataset comprises three tibbles:

- `patient_status`: Designed for ease of use with ggswim, it may not
  perfectly mirror real-world data, but serves as an ideal starting
  point. A 7 column tibble with subject cohorts, statuses, and start and
  end times.
- `adverse_events`: A 3 column tibble showcasing 3 kinds of adverse
  events and the time when they occurred
- `medication_administration`: A 4 column tibble showcasing 2 kinds of
  medications and the time they were given

You can also access each of these tibbles individually. Let’s load the
data and dive into creating our swimmer plots!

``` r
library(ggswim)

ggswim(
  data = patient_status,
  mapping = aes(
    x = value,
    y = subject_id,
    fill = cohort
  ),
  arrow = alive
) +
  add_marker(
    data = adverse_events,
    mapping = aes(
      x = time_of_event,
      y = subject_id,
      color = adverse_event_name,
      shape = adverse_event_name
    ),
    size = 5
  ) +
  add_marker(
    data = medication_administration,
    mapping = aes(
      x = time_of_event,
      y = subject_id,
      label = medication,
      color = name
    ),
    label.size = NA, fill = NA, size = 5
  ) +
  ggplot2::labs(x = "Time", y = "Subject ID", color = "Markers") +
  ggplot2::ggtitle("My Swim Plot") +
  ggplot2::scale_color_manual(
    name = "Markers",
    values = c("firebrick", "forestgreen", NA, NA, "purple")
  ) +
  ggplot2::scale_shape_manual(
    name = "Markers",
    values = c(19, 18, 15)
  ) +
  ggplot2::scale_fill_manual(
    name = "Lanes",
    values = c("steelblue1", "goldenrod1")
  ) +
  theme_ggswim()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
