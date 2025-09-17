
# Lab3PonOlAroEn

<!-- badges: start -->
[![R-CMD-check](https://github.com/arreen/Lab3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/arreen/Lab3/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of Lab3PonOlAroEn is to program an euclidean and dijkstra algorithm and make it a R package.

## Installation

You can install the development version of Lab3PonOlAroEn from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("arreen/Lab3")
```

## Example

Here is a short showcase of the functions
``` r
library(Lab3PonOlAroEn)
## Showcase of Euclidean()
 euclidean(123612, 13892347912)

## Showcase of Dijkstra
data(wiki_graph)
dijkstra(wiki_graph, 1)
```

