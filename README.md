dockr
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/JBGruber/dockr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JBGruber/dockr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/JBGruber/dockr/branch/main/graph/badge.svg)](https://codecov.io/gh/JBGruber/dockr?branch=main)
<!-- badges: end -->

The goal of dockr is to provide access to the Docker API using only R
(no Python, Go or system calls). The main use case so far is to parse
Docker Compose files, pull create and run images and control them. Not
all compose options are implemented yet!

## Installation

You can install the development version of dockr like so:

``` r
remotes::install_github("JBGruber/dockr")
```

## Example

Say you have a Docker Compose file that spins up several containers. The
package includes a very simple one for testing:

``` r
readLines(system.file("extdata/docker-compose.yml", package = "dockr")) |> 
  paste(collapse = "\n") |> 
  cat()
#> services:
#>   test:
#>     image: "redis:alpine"
#>     container_name: "test"
#>     ports:
#>       - "8000:5000"
#>     command: ["sleep", "120"]
#> 
#> networks:
#>   test-net:
```

You can start the container defined here by:

``` r
library(dockr)
compose_up(compose = system.file("extdata/docker-compose.yml", package = "dockr"))
#> test started
#> all done!
```

To check on your containers, you can use:

``` r
docker_lc()
#> # A tibble: 1 × 5
#>   name  image        status                id                              ports
#>   <chr> <chr>        <chr>                 <chr>                           <chr>
#> 1 test  redis:alpine Up Less than a second a1e0a05551ed33ecb5e371d0e340bc… "lis…
```

To stop and remove this container and the underlying image, use:

``` r
# stop container
docker_stop("test")
#> test stopped
# remove container
docker_rmc("test")
# remove image
docker_rmi("redis:alpine")
```
