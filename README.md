
# Lab3PonOlAroEn

<!-- badges: start -->
[![R-CMD-check](https://github.com/arreen/Lab3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/arreen/Lab3/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of Lab3PonOlAroEn is to program an euclidean and dijkstra algorithm and make it into a R package.

## Installation

To install the package run the following code:

``` r
# install.packages("devtools")
devtools::install_github("arreen/Lab3PonOlAroEn")
library(Lab3PonOlAroEn)
```

You can also install the development version of Lab3PonOlAroEn from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("arreen/Lab3")
```

## Example

Here is a short showcase of the functions
``` r
library(Lab3PonOlAroEn)
## Showcase of euclidean(). This function finds the greatest common divisor between two whole numbers.
 euclidean(123612, 13892347912)

## Showcase of dijkstra(). This function finds the shortest path from one node to every other node in a weighted graph.
data(wiki_graph)
dijkstra(wiki_graph, 1)
```

