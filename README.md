
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

ggswim helps create stunning swimmer plots by integrating with the
familiar ggplot2 framework, allowing developers to effortlessly add
layers and customize displays to their needs.

ggswim streamlines the process of generating legends that clearly
communicate layer data of added complexity when combining fill, point,
and label layer types.

Let’s dive right into a quick example to showcase the simplicity and
effectiveness of ggswim!

## Exploring the Sample Dataset & Creating a Swimmer Plot

To help you get started, ggswim includes three sample datasets:
`patient_data`, `infusion_events`, and `end_study_events`. These
de-identified datasets simulate real world data related to infusions,
disease assessments, and study statuses for a clinical trial.

Let’s load the data and dive into creating our swimmer plots! The first
step is to create a variant of a `geom_col()` using the `ggswim()`
function. This makes the swimmer plot “lanes”:

``` r
library(ggswim)
library(ggplot2)

p <- ggswim(
  patient_data |> dplyr::rename("Status Markers" = bcell_status),
  mapping = aes(x = delta_t0_months, y = pt_id, fill = disease_assessment_status),
  arrow = arrow_status,
  arrow_head_length = unit(.25, "inches"),
  arrow_neck_length = delta_today,
  width = 0.25
)
```

Next we’ll add on events of interest, what we’ll refer to as “markers”:

``` r
p <- p +
  add_marker(
    aes(x = delta_t0_months, y = pt_id, color = `Status Markers`, shape = `Status Markers`),
    size = 5, position = "identity", alpha = 1
  ) +
  add_marker(
    data = end_study_events,
    aes(x = delta_t0_months, y = pt_id, label_vals = end_study_label, label_names = end_study_name),
    label.size = NA, fill = NA, size = 5
  ) +
  add_marker(
    data = infusion_events,
    aes(x = infusion_delta_t0_months, y = pt_id, color = infusion_type, shape = infusion_type),
    size = 5, position = "identity", alpha = 1
  ) 
```

And finally, we’ll spruce up our plot a bit to make it more
aesthetically appealing:

``` r
p +
  scale_colour_manual(
    values = c("#b22228", "#F5EB0A", "gray50", NA, NA, NA, "#25DA6D", "#25DA6D")
  ) +
  scale_shape_manual(
    values = c(19, 19, 15, 17, 18)
  ) +
  scale_fill_manual(
    name = "Overall Disease Assessment",
    values = c("#6394F3", "#F3C363", "#EB792F")
  ) +
  labs(title = "Sample Swimmer Plot") +
  xlab("Time (Months)") + ylab("Patient ID") +
  theme_ggswim()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />
