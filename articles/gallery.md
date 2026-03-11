# Gallery

``` r
library(ggswim)
library(ggplot2)
library(dplyr)
```

## Random Data Sets

In this example, we’ll set up some random data for reproducibility by
defining dataframes for our lanes and our markers.

``` r
set.seed(123)
lane_data <- tibble(
  x = 0,
  xend = sample(5:20, 30, replace = TRUE),
  y = factor(rep(1:15, each = 2)),
  colour = sample(c("red", "blue", "green", "yellow", "purple"), 30, replace = TRUE)
)

set.seed(123)
marker_data <- tibble(
  x = sample(5:20, 30, replace = TRUE),
  y = factor(rep(1:15, each = 2)),
  label = sample(c("A", "B", "C", "D", "E"), 30, replace = TRUE),
  glyph = NA
) |>
  mutate(
    glyph = dplyr::case_when(
      label == "A" ~ "😊",
      label == "B" ~ "🎉",
      label == "C" ~ "✅",
      label == "D" ~ "💥",
      label == "E" ~ "✨",
      .default = NA
    )
  )
```

And then we’ll call those dataframes into their appropriate swim and
marker geom functions:

``` r
ggplot() +
  geom_swim_lane(
    data = lane_data,
    aes(x = x, xend = xend, y = y, colour = colour),
    linewidth = 3
  ) +
  geom_swim_marker(
    data = marker_data,
    aes(x = x, y = y, marker = label),
    size = 8
  ) +
  scale_colour_brewer(name = "Lanes", palette = "Set1") +
  with(
    marker_data,
    scale_marker_discrete(glyphs = glyph, limits = label, name = "Markers")
  ) +
  labs(
    title = "Sample Swimmer Plot",
    x = "Time", y = "Record ID"
  ) +
  theme_ggswim()
```

![A swimmer plot displaying use of emojis for event
markers.](gallery_files/figure-html/unnamed-chunk-3-1.png)

## Using FontAwesome Icons

Next, we’ll replace the emojis above with calls to
[`fontawesome()`](https://chop-cgtinformatics.github.io/ggswim/reference/fontawesome.md)
icons after first loading fonts with
[`load_fonts()`](https://chop-cgtinformatics.github.io/ggswim/reference/load_fonts.md):

``` r
# Load fonts from the ggswim GitHub repository
load_fonts(verbose = FALSE)

marker_data <- marker_data |>
  mutate(
    glyph = dplyr::case_when(
      label == "A" ~ fontawesome("fa-car"),
      label == "B" ~ fontawesome("fa-check"),
      label == "C" ~ fontawesome("fa-user"),
      label == "D" ~ fontawesome("fa-cat"),
      label == "E" ~ fontawesome("fa-dog"),
      .default = NA
    )
  )

ggplot() +
  geom_swim_lane(
    data = lane_data,
    aes(x = x, xend = xend, y = y, colour = colour),
    linewidth = 3
  ) +
  geom_swim_marker(
    data = marker_data,
    aes(x = x, y = y, marker = label),
    size = 8, family = "FontAwesome-Solid"
  ) +
  scale_colour_brewer(name = "Lanes", palette = "Set1") +
  with(
    marker_data,
    scale_marker_discrete(glyphs = glyph, limits = label, name = "Markers")
  ) +
  labs(
    title = "Sample Swimmer Plot",
    x = "Time", y = "Record ID"
  ) +
  theme_ggswim()
```

![A swimmer plot displaying use of FontAwesome icons for event
markers.](gallery_files/figure-html/unnamed-chunk-4-1.png)

Be sure to specify the appropriate `family` argument in
[`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md).
For FontAwesome the following are available:

- “FontAwesome-Solid”
- “FontAwesome-Regular”
- “FontAwesome-Brands”

You can use
[`search_fontawesome()`](https://chop-cgtinformatics.github.io/ggswim/reference/search_fontawesome.md)
to check what icons are available to use.

> ggswim supports FontAwesome free icons through the open source
> [license](https://fontawesome.com/license/free).

## Using Bootstrap Icons

We can similarly use Bootstrap icons with
[`bootstrap()`](https://chop-cgtinformatics.github.io/ggswim/reference/bootstrap.md):

``` r
marker_data <- marker_data |>
  mutate(
    glyph = dplyr::case_when(
      label == "A" ~ bootstrap("bs-car-front"),
      label == "B" ~ bootstrap("bs-folder-fill"),
      label == "C" ~ bootstrap("bs-clock-fill"),
      label == "D" ~ bootstrap("bs-check-circle-fill"),
      label == "E" ~ bootstrap("bs-chat-fill"),
      .default = NA
    )
  )

ggplot() +
  geom_swim_lane(
    data = lane_data,
    aes(x = x, xend = xend, y = y, colour = colour),
    linewidth = 3
  ) +
  geom_swim_marker(
    data = marker_data,
    aes(x = x, y = y, marker = label),
    size = 8, family = "Bootstrap"
  ) +
  scale_colour_brewer(name = "Lanes", palette = "Set1") +
  with(
    marker_data,
    scale_marker_discrete(glyphs = glyph, limits = label, name = "Markers")
  ) +
  labs(
    title = "Sample Swimmer Plot",
    x = "Time", y = "Record ID"
  ) +
  theme_ggswim()
```

![A swimmer plot displaying use of Bootstrap icons for event
markers.](gallery_files/figure-html/unnamed-chunk-5-1.png)

Be sure to specify the appropriate `family` argument in
[`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md).
For Bootstrap you only need to specify “Bootstrap”.

You can use
[`search_bootstrap()`](https://chop-cgtinformatics.github.io/ggswim/reference/search_bootstrap.md)
to check what icons are available to use.

> ggswim supports Bootstrap free icons through the open source
> [license](https://github.com/twbs/bootstrap/blob/main/LICENSE).

## Themeing with ggswim

Here we’ll demonstrate some of the theme functions available with
ggswim. These examples will use the same plot setup from the README,
starting with the original output:

``` r
p
```

![A swimmer plot displaying use of the default datasets in
ggswim.](gallery_files/figure-html/unnamed-chunk-7-1.png)

### `theme_ggswim()`

``` r
p + theme_ggswim()
```

![A swimmer plot displaying use of the default datasets in ggswim and
updated with
theme_ggswim().](gallery_files/figure-html/unnamed-chunk-8-1.png)

### `theme_ggswim_dark()`

``` r
p + theme_ggswim_dark()
```

![A swimmer plot displaying use of the default datasets in ggswim and
updated with
theme_ggswim_dark().](gallery_files/figure-html/unnamed-chunk-9-1.png)
