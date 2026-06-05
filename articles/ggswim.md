# Getting Started with ggswim

The ggswim package helps you build swimmer plots in R with ggplot2-style
layers. This vignette walks through the main workflow: adding patient
timelines, placing events on those timelines, styling marker legends,
and applying final plot polish.

ggswim offers several functions that fit into familiar ggplot2
workflows:

- [`geom_swim_lane()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_lane.md)
- [`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md)
- [`scale_marker_discrete()`](https://chop-cgtinformatics.github.io/ggswim/reference/scale_marker_discrete.md)
- [`geom_swim_arrow()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_arrow.md)
- [`scale_arrow_discrete()`](https://chop-cgtinformatics.github.io/ggswim/reference/scale_arrow_discrete.md)
- [`theme_ggswim()`](https://chop-cgtinformatics.github.io/ggswim/reference/theme_ggswim.md)

[`geom_swim_lane()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_lane.md),
an extension of
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html),
constructs the horizontal timelines, which we’ll refer to as “lanes.”
[`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md)
wraps
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
to place key events of interest, or “markers,” on those lanes. These
markers can take the form of shapes, symbols, or emojis. Arrows and
themes are covered briefly here, with more examples in the focused
articles.

Drawing from the well-established principles of ggplot2, ggswim lets
users apply familiar layer-building techniques, including scales,
labels, and themes.

### Adding a Lane Layer

Let’s get started building our swimmer plot! As with the README, we will
be using ggswim’s internal datasets: `patient_data`, `infusion_events`,
and `end_study_events`.

Let’s start with observing `patient_data`’s structure:

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

`patient_data` contains a long dataset where patient IDs (`pt_id`) can
be repeated. These rows are differentiated by `disease_assessment`
values combined with corresponding start and end times, measured in
months. Together, these rows detail clinical trial timelines for each
patient.

- `disease_assessment` is broken down into a few categories where:
  - CR = “Complete Response”
  - CRi = “Complete Response with Incomplete Blood Count Recovery”
  - RD = “Relapsed Disease”

Now, let’s make the plot using
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) and
our first layer with
[`geom_swim_lane()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_lane.md):

``` r

library(ggplot2)

p <- ggplot() +
  geom_swim_lane(
    data = patient_data,
    mapping = aes(
      x = start_time,
      xend = end_time,
      y = pt_id,
      color = disease_assessment
    ),
    linewidth = 3
  )

p
```

![Initial swimmer plot with
lanes.](ggswim_files/figure-html/ggswim%20plot-1.png)

Here we have patient timelines split into disease assessment intervals.
[`geom_swim_lane()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_lane.md)
does the work of setting up
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
and readying our plot layers for additional ggswim-specific features
such as the “markers” mentioned earlier. It’s worth noting that
[`geom_swim_lane()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_lane.md)
is a thin wrapper around
[`geom_segment()`](https://ggplot2.tidyverse.org/reference/geom_segment.html)
and supports the same functionality apart from a `yend` since swimmer
plots tend to be horizontal.

## Adding a Marker Layer: Points

Now, let’s add a marker layer by first inspecting the `infusion_events`
and `end_study_events` datasets:

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

This dataset is much simpler, indicating the time from the initial
infusion (0), and reinfusions at some point beyond 0 (if a patient had
any). These are also categorized under `label`. `glyph` and `colour`
will serve as helpful specifiers when we add our markers onto our
swimmer plot. Initial infusions aren’t a part of this dataset since the
plot itself is centered around them, and having markers for all segments
at month 0 would be relatively meaningless.

Next, let’s look at our end of study events, i.e. events that indicate a
patient has left the study for various reasons.

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

You’ll notice that this dataset includes emojis under `glyph`. In
addition to shapes and symbols, ggswim supports emojis when rendering
swimmer plots. If emojis or custom shapes do not render as expected,
check your graphics device settings; AGG devices usually work well.

While it’s common to encounter these as separate datasets in the wild,
it will make our lives much easier to combine `end_study_events` and
`infusion_events` together since they share roughly the same data
structure and the markers exist on the same timeline.

``` r

all_events <- dplyr::bind_rows(
  infusion_events,
  end_study_events
)

all_events
#> # A tibble: 25 × 5
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
#> # ℹ 15 more rows
```

Let’s now call
[`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md)
and add the events onto our plot. To do this, we will use
[`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md)’s
custom `marker`
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) parameter:

``` r

p <- p +
  geom_swim_marker(
    data = all_events,
    aes(
      x = time_from_initial_infusion,
      y = pt_id,
      marker = label
    ),
    size = 4
  )

p
```

![Updated swimmer plot with
markers.](ggswim_files/figure-html/second%20marker%20plot-1.png)

We’ve made a swimmer plot with lanes and two kinds of events. Notice how
even though both the lanes and markers use colour internally, they are
separated in the legend output. Let’s take it one step further and make
use of the `glyph` and `colour` columns we specified.

## A Sense of Scale

Scales are a unique component of the ggplot2 framework. For more
information on them and understanding how they work, you are encouraged
to read the “Scales” chapter from [ggplot2: Elegant Graphics for Data
Analysis](https://ggplot2-book.org/scales). In short, scales are
responsible for connecting data with aesthetics and communicating those
connections through elements like the plot legend.

[`scale_marker_discrete()`](https://chop-cgtinformatics.github.io/ggswim/reference/scale_marker_discrete.md)
makes it easy to control marker appearance in the plot and legend. This
is helpful because plain
[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
can show event labels, but it does not give ggswim’s separate marker
legend behavior:

``` r

ggplot() +
  geom_text(
    data = all_events,
    aes(x = time_from_initial_infusion, y = pt_id, label = label, colour = glyph),
    size = 4
  )
```

![Example display of markers without scale_marker_discrete, leading to
incorrect output.](ggswim_files/figure-html/unnamed-chunk-2-1.png)

[`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html)
is useful, and it is what
[`geom_swim_marker()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_marker.md)
wraps, but it does not connect event labels, display glyphs, and legend
entries the way swimmer plots often need.

Thanks to ggswim we can specify what belongs in the glyph versus text
elements of our legend. Additionally, we are able to use the “colour”
scales under the `marker` definition so ggswim can use the same colour
scale under the hood, but keep the identity of “markers” and “lanes”
separate in the output:

``` r

p <- p +
  scale_marker_discrete(
    name = "Study Events",
    glyphs = all_events$glyph,
    colours = all_events$colour,
    limits = all_events$label
  )

p
```

![Updated swimmer plot with correctly used scale_marker_discrete for
marker output.](ggswim_files/figure-html/unnamed-chunk-3-1.png)

## A Full Swimmer Plot

As mentioned in the README, all of the same ggplot2 techniques you would
apply to your usual plots apply here. Below, we update the lanes to
match a nicer palette with a new name and add on some plot labels:

``` r

library(ggplot2)

p <- p +
  theme_minimal() +
  scale_color_brewer(
    name = "Disease Assessments",
    palette = "Set1"
  ) +
  labs(title = "My Swimmer Plot") +
  xlab("Time (Months)") + ylab("Patient ID")

p
```

![Updated swimmer plot with updated color
scales.](ggswim_files/figure-html/mapping%20finalizing-1.png)

We can also apply
[`theme_ggswim()`](https://chop-cgtinformatics.github.io/ggswim/reference/theme_ggswim.md)
for a swimmer-plot-oriented theme:

``` r

p +
  theme_ggswim()
```

![Updated swimmer plot with
theming.](ggswim_files/figure-html/mapping%20with%20theme_ggswim-1.png)

### Additional notes

Some additional considerations to keep in mind when working with ggswim:

- **Rendering Emojis and Custom Shapes:** To ensure emojis and other
  custom shapes display correctly, users may need to [switch their
  graphics rendering
  device](https://support.posit.co/hc/en-us/articles/360017886674-Troubleshooting-RStudio-Rendering-Errors)
  to AGG.
- ggswim supports use of FontAwesome and Bootstrap icons for glyph
  definition in addition to shapes and emojis. Check out the Gallery for
  examples on how to add them to your plots!
- See the Adding arrows article for a focused guide to continuation
  indicators with
  [`geom_swim_arrow()`](https://chop-cgtinformatics.github.io/ggswim/reference/geom_swim_arrow.md)
  and
  [`scale_arrow_discrete()`](https://chop-cgtinformatics.github.io/ggswim/reference/scale_arrow_discrete.md).
