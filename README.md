
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A package implementing Taylor diagrams using ggplot2

The goal of ggtaylor is to implement the ability to generate Taylor
Diagrams (Taylor, 2001) as a ggplot2 geom.

I spent a few hours building this out because 1) I couldn’t find any
other implementations and 2) I wanted to start exeperimenting with the
modern package development suite of tools (especially the usethis
package). I thought this was going to be easy, but it got very complex
very quickly!

There are already implementations in the
[plotrix](https://rdrr.io/cran/plotrix/man/taylor.diagram.html), and
[openair](https://davidcarslaw.github.io/openair/reference/TaylorDiagram.html).

Not sure when I will get back to it again but **Contributions
Welcome!!**

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of ggtaylor like so:

``` r
#pak::pkg_install("dlebauer/ggtaylor")
devtools::load_all()
```

## Example

This is a basic example of the intended use:

``` r
library(ggtaylor)
# Create the plot
d <- data.frame(
  obs = rnorm(100),
  mod = rnorm(100),
  model = rep(c("A", "B"), 50)
)
ggplot(data = d) +
  geom_taylor(aes(x = obs, y = mod, color = model)) +
  scale_x_continuous(name = "Correlation", breaks = seq(0, 1, by = 0.2),
                     labels = function(x) round(cos(x * pi/2), 2)) +
  scale_y_continuous(name = "Standard Deviation") +
  theme_minimal()
```
