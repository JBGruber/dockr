My Project
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# dockr

<!-- badges: start -->
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
  cat()
#> services:   test:     image: "redis:alpine"     container_name: "test"     ports:       - "8000:5000"     command: ["sleep", "120"]  networks:   test-net:
```

You can start the container defined here by:

``` r
library(dockr)
compose_up(compose = system.file("extdata/docker-compose.yml", package = "dockr"))
#> container already started
#> all done!
```

To check on your containers, you can use:

``` r
docker_lc()
#> # A tibble: 1 × 5
#>   name  image        status       id                                       ports
#>   <chr> <chr>        <chr>        <chr>                                    <chr>
#> 1 test  redis:alpine Up 7 seconds cbdd89fce5df3cc18f2a366fd35483536054cfa… "lis…
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
