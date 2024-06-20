
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biroot

**Note: this package is currently under construction. You should not use
it.**
<!-- extends [**ggplot2**](https://github.com/tidyverse/ggplot2) providing -->
<!-- functions to visualize the decision spaces of classification and clustering methods. -->
<!-- **ggclassify** currently includes the functions `geom_classify()` and `geom_classify_boundary()` for plotting the classification regions through filling in the regions, or outlining the resulting -->
<!-- boundaries respectively. -->

## Basic usage

**biroot** supplies tools for finding the one dimensional root of a
function of two variables over a fixed rectangular spatial extent. For
example, if $f(x,y) = x^{2} + y^{2} - 1$ is the function of interest and
$\mathcal{D} = [-2,2] \times [-2,2] \subset \mathbb{R}^{2}$ is the
spatial extent, the goal of these tools would be to find that the set
$\{(x,y) \in \mathcal{D}: f(x,y) = 0\}$ is the unit circle and provide a
data frame parameterizing points on it.

A few notes are worth mentioning.

1.  As is typical in R, the function $f(x,y)$ is not implemented as
    `f(x,y)` but rather `f(v)`, where `v = c(x,y)`.
2.  Points $(x,y)$ such that $f(x,y) = 0$ are referred to as *solutions
    of $f$*, and the collection of such points is the level set of $f$
    at $0$. For an arbitrary $f$, the level set of $f$ at $0$ can be
    quite complicated: it can have isolated solutions (“0d” solutions,
    e.g. $f(x,y) = x^{2} + y^{2}$), curves of solutions (“1d” solutions,
    e.g. $f(x,y) = x^{2} + y^{2} - 1$), spatial extents of solutions
    (“2d” solutions, e.g. $f(x,y) = 1[x^{2} + y^{2} \leq 1]$), or no
    solutions (e.g. $f(x,y) = x^{2} + y^{2} + 1$). **biroot** is only
    defined with 1d solutions in mind.
3.  1d solution sets can be complicated. They can be disconnected
    (e.g. $f(x,y) = ((x-2)^{2} + y^{2} - 1)((x+2)^{2} + y^{2} - 1)$),
    unbounded (e.g. $f(x,y) = y - x$), self intersecting
    (e.g. $f(x,y) = (y-x)(y+x)$), or exhibit other cusp-like behavior
    (e.g. $f(x,y) = y^{2} - x^{3}$).

The basic algorithm used in biroot is [the quadtree
algorithm](https://en.wikipedia.org/wiki/Quadtree), of which there are
many variations depending on use casee but here can be seen to be an
adaptive grid search, similar to a 2d analogue of the [bisection
method](https://en.wikipedia.org/wiki/Bisection_method). It has two core
functions:

1.  `biroot()`, which accepts a function `f` and spatial extents `xlim`
    and `ylim` (numeric vectors of length 2). This function is called
    `biroot(f, xlim, ylim)` and returns a data frame of places (`x` and
    `y`) where `f` was evaluated in the Cartesian product of `xlim` and
    `ylim`, the value of the function at those points (`value`), along
    with other information corresponding to the algorithm (`depth`, or
    which scale, and `id`, which rectangle at that scale).
2.  `biroot_lines()` and `biroot_bands()`, made by analogy to
    `isolines()` and `isobands()` from [the **isoband**
    package](https://isoband.r-lib.org) and having a similar data
    structure.

Here is an example of the basic use of these functions.

``` r
library("biroot")

f <- function(v) with(v, x^2 + y^2 - 1)
df <- biroot(f, xlim = c(-2,2), ylim = c(-2,2)) 
str(df)
#> 'data.frame':    33300 obs. of  5 variables:
#>  $ x    : num  -2 -2 2 2 -2 -2 0 0 0 0 ...
#>  $ y    : num  -2 2 2 -2 0 2 2 0 0 2 ...
#>  $ id   : chr  "0" "0" "0" "0" ...
#>  $ depth: num  0 0 0 0 1 1 1 1 1 1 ...
#>  $ value: num  7 7 7 7 3 7 3 -1 -1 3 ...

color_function <- function(x) {
  f <- colorRampPalette(c("gray85", "firebrick1", "gray85"))(10) |> colorRamp()
  
  threshold_to_interval <- function(x, l, u) l*(x <= l) + x*(l < x & x < u) + u*(x >= u)
  
  rescale <- function(x) (x - min(x)) / (max(x) - min(x))
  
  x |> 
    threshold_to_interval(l = -1, u = 1) |> 
    rescale() |> 
    f() |> 
    apply(1, \(v) rgb(v[1], v[2], v[3], maxColorValue = 255))
}

plot(x, y, bg = color_function(value), pch = 21, col = "gray25") |> 
  with(df, expr = _)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## `biroot_lines()`

`biroot_lines()` takes the same arguments as `biroot()`, but instead of
returning the points found by biroot, it takes them and runs marching
squares on each square individually and returns a dataframe with a pair
of points defining a line using the basic 15 case algorithm described in
`https://en.wikipedia.org/wiki/Marching_squares`. Each line has a unique
id inherited from the square it crosses. Any squares that don’t have a
line due to being all negative or positive are filtered out.

``` r
library("ggplot2"); theme_set(theme_minimal())
#> Warning: package 'ggplot2' was built under R version 4.3.2
theme_update(panel.grid.minor = element_blank())
library("patchwork")

plot_contour_at_depth <- function(depth, xlim = c(-2, 2), ylim = c(-2, 2), min_depth = 2) {
  if (length(depth) > 1) 
      return(
        depth |> 
          lapply(plot_contour_at_depth, xlim, ylim, min_depth) |> 
          Reduce(`+`, x = _)
      )
  
  biroot_lines(f, xlim, ylim, max_depth = depth, min_depth = min_depth) |> 
    ggplot(aes(x, y, group = id)) +
      geom_point(
        aes(x, y), size = .1, color = "gray65", inherit.aes = FALSE, 
        data = biroot(f, xlim, ylim, depth, min_depth = min_depth)
      ) +
      geom_line(color = "firebrick1") +
      theme(
        axis.title = element_blank(), axis.text = element_blank(), 
        panel.grid = element_blank(), panel.background = element_rect(color = "gray65")
      ) +
      coord_equal(xlim = xlim, ylim = ylim)
}

p <- mpoly::mp("(x^2 + y^2 - 1)^3 - x^2 y^3")
pf <- as.function(p, silent = TRUE)
f <- function(df) with(df, cbind(x, y) |> pf())
plot_contour_at_depth(1:9, min_depth = 0)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Note that if the level set is complex, the relationship between
`min_depth` and `max_depth` is very significant at “getting” the level
set.

``` r
(p <- mpoly::lissajous(7, 7,  0, 0))
#> 49 x^2  -  784 x^4  +  4704 x^6  -  13440 x^8  +  19712 x^10  -  14336 x^12  +  4096 x^14  +  49 y^2  -  784 y^4  +  4704 y^6  -  13440 y^8  +  19712 y^10  -  14336 y^12  +  4096 y^14  -  1
pf <- as.function(p, silent = TRUE)
f <- function(df) with(df, cbind(x, y) |> pf())
plot_contour_at_depth(3:6, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r


plot_contour_at_depth(8, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1))
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

``` r
plot_contour_at_depth(8, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), min_depth = 4)
```

<img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" />

``` r
plot_contour_at_depth(8, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), min_depth = 5)
```

<img src="man/figures/README-unnamed-chunk-5-4.png" width="100%" />

## Installation

You can install the latest development version of **ggclassify** from
[GitHub](https://github.com/) with:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("nathaniel-morgan/biroot")
```
