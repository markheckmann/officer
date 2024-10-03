library(officer)

# 1 + 2
x <- read_pptx()
layout_summary(x)
plot_layout_properties(x, 1, legend = TRUE)

# 3
x <- add_slide(x, "Comparison")
plot_layout_properties(x)

-----------------------------------

Feat: `plot_layout_properties()`: accept index, new legend arg, plot current slide by default

### Changes in `plot_layout_properties()` (#595)

1. Now accepts the layout index (see `layout_summary()`) as an alternative to the layout name.
2. Gains an argument `legend` to add a legend to the plot.
3. Plots the current slide's layout by default, if no layout name is provided explicitly.

----
### 1 + 2 Layout index, `legend` arg

``` r
library(officer)

x <- read_pptx()
layout_summary(x)
#>              layout       master
#> 1       Title Slide Office Theme
#> 2 Title and Content Office Theme
#> 3    Section Header Office Theme
#> 4       Two Content Office Theme
#> 5        Comparison Office Theme
#> 6        Title Only Office Theme
#> 7             Blank Office Theme
plot_layout_properties(x, 1, legend = TRUE)  # layout index (see above), legend arg
```

![](https://i.imgur.com/G5CSbgl.png)<!-- -->

### 3 Plot current slide's layout by default

Without a layout, `plot_layout_properties` now plots the current slide's layout and informs the user.

``` r
x <- add_slide(x, "Comparison")
plot_layout_properties(x)
#> ℹ Showing current slide's layout: "Comparison"
```

![](https://i.imgur.com/sQOfFeU.png)<!-- -->

  <sup>Created on 2024-10-03 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

