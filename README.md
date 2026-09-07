# prettyODS - Creating ODS files using R

<!-- badges: start -->
<!-- badges: end -->

There is currently no straightforward way to create ODS files from R with full control over text styles, sheet styles, and other formatting.
**prettyODS** is an early prototype and the first step toward a fully featured R package that provides these capabilities.

## Installation

You can install the development version of prettyODS from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Jakob256/prettyODS")
```

## Example:

``` r
library(prettyODS)

orangeStyle <- ODS_createStyle(font="Arial", size=20, italic=TRUE, rotate=20, color="orange")
sheet <- ODS_createSheet("my first ODS sheet")

ODS_writeCell(sheet, "Hello World!", row=1, col=1, orangeStyle)
ODS_write(sheet, "hello.ods")
```

<p align="center" width="100%">	
    <img width="40%" src="man/figures/helloWorld.png">
</p>
