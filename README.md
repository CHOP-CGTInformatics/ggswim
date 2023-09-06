
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

## Background

At it’s core, the ggswim package leverages the already fantastic
capabilities of ggplot2 and seeks to preserve as much of the flexibility
and structure that it provides as possible. Therefore, when using ggswim
you will likely notice that much of the documentation and underlying
functionality wrap it! So what makes it different? ggswim provides some
opinionated decisions to steer plots and the data provided in a way that
makes swimmer plots easily accessible and easy to build upon.

Let’s start with some simple examples of how this works.

## A Sample Data Set

First we’ll define a few sets of data to work with:

- `patient_data`: a dataframe containing per-patient, record-level data
- `dose_data_a`: a dataframe with two doses corresponding to multiple
  potential time points for the patients in `patient_data`

``` r
set.seed(123)
patient_data <-
  tibble::tibble(
    id = 1:4,
    trt = rep_len(c("Drug A", "Drug B"), length.out = 4),
    time_to_last_followup = c(5,2,4,7),
    time_to_death = ifelse(id %% 2, time_to_last_followup, NA),
    end_time = c(5,2,4,7)
  )

dose_data_a <- tibble::tibble(
  id2 = c(1,1,1,2,2,2,3,4,4,4),
  type = sample(c("Dose I", "Dose II"), 10, replace = TRUE),
  time = c(0,1.5,2,0,0.5,1,1.25,2,3,7)
)
```

Now, let’s give `patient_data` a start time of 0 and pivot the table to
assign time frames under a `time` column:

``` r
patient_data <- patient_data |>
  dplyr::mutate(time_start = 0) |>
  tidyr::pivot_longer(cols = c(time_start, time_to_last_followup),
                      values_to = "time",
                      names_to = "treatment_group")
```

We’re ready to swim! `ggswim()` requires the following mapping
aesthetics: `x`, `y`, and `fill` and we’ll tack on a static `width` for
good measure:

``` r
library(ggswim)
library(ggplot2)

p <- patient_data |> 
  ggswim(
    aes(y = id,
        x = time,
        fill = trt), width = 0.1
  )

p
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Awesome! Now let’s throw on some event-specific points, what we’ll call
“markers,” given to us in `dose_data_a`. Notice here that we can add the
new data just like a regular ggplot2 series of calls. You’ll also notice
the colors aren’t quite what we want right now, but we’ll resolve that
later.

`add_marker()`, like `ggswim()`, has a set of requires aesthetic
mappings as well: `x`, `y`, and `color`. But you can provide others as
well, like `shape`.

``` r
p <- p +
  add_marker(data = dose_data_a,
             mapping = aes(
               x = time,
               y = id2,
               shape = type,
               color = type
             ), size = 3)

p
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

What if we have multiple marker datasets? Let’s define and tack on
`dose_data_b`:

``` r
dose_data_b <- tibble::tibble(
  id3 = c(1,1,2,3,4),
  type2 = sample(c("Dose III", "Dose IV"), 5, replace = TRUE),
  time = c(0.5, 0.75, 0.25, 3, 6)
)

p <- p +
  add_marker(data = dose_data_b,
             mapping = aes(
               x = time,
               y = id3,
               shape = type2,
               color = type2,
             ), size = 5)

p
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Voila! The next set of markers has been added.

## Adding Labels and Emojis

Let’s get extra fancy, let’s say instead of using shapes to depict
markers we want to use emojis. `add_marker()` supports those as well! So
long as an additional column is provided to the required aesthetic
mapping `label`. We’ll showcase this using a new data set called
`dose_type` to add on to our existing plot:

``` r
dose_type <- tibble::tibble(
  id4 = c(1,2,3,4),
  label = c("💊", "💉", "💊", "💉"),
  name = c("Method A", "Method B", "Method A", "Method B"),
  time = c(.15, 0.1, 2.25, 5.5)
)

p <- p + 
  add_marker(
    dose_type,
    aes(x = time,
        y = id4,
        label = label,
        color = name),
    label.size = NA, fill = NA, size = 5
  )

p
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Wow! … I think? It’s pretty busy and a little hard to follow. We will
discuss more about why this is in an upcoming vignette, but for now we
provide you with an additional function to sort this all out:
`fix_legend()`. `fix_legend()` must be used on a rendered ggplot object,
it will not work with the `+` operator.

``` r
fix_legend(p)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

And now you can see that all of the emojis and shapes are correctly
assigned in the legend! Now, about those colors and this ugly display….

## Making the Plot Pretty

Recall earlier that we said ggswim makes use of existing ggplot2
architecture. This means you can use your favorite manipulators to make
your ggswim objects look however you please. Below, we take the
pre-existing plot, `p`, and supply some beautifucation steps before
sending it to `fix_legend()`:

``` r
p <- p +
  ggplot2::xlab("Time") + ggplot2::ylab("Subject ID") +
  ggplot2::ggtitle("My Swim Plot") +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(name = "Markers",
                              values = c("firebrick", "tomato", "orange", "chartreuse2", NA, NA)) +
  ggplot2::scale_shape_manual(name = "Markers",
                              values = c(19,15,8,18)) +
  ggplot2::scale_fill_manual(name = "Lanes",
                             values = c("steelblue", "cyan", "skyblue", "steelblue4"))


fix_legend(p)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

You’ll notice in the call to `scale_color_manual()` we had to supply
`NA` values for the labels, we explain that more in the upcoming
vignette and hope to provide a better handling method in the future.

### Future Plans

We have a lot of work to do with ggswim! Future plans include:

\[ \] A full test suite \[ \] Enforced checks \[ \] Handling of fixed
legend scales’ \[ \] Vignettes and documentation
