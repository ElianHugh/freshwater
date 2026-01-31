# 🌿💧 freshwater

> \[!NOTE\] This package is under active development and its
> functionality may change over time.

freshwater provides server-side rendering utilities for plumber2
backends:

- composable HTML templates
- template caching[¹](#fn1)
- weak ETag caching
- shiny tag serialisation[²](#fn2)

## Installation

You can install the development version of freshwater from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ElianHugh/freshwater")
```

## Example

``` r
library(freshwater)

page_main <- template(
    year = 2026, {
        div(
            h1("Dashboard"),
            fragment(p("Welcome back"), name = "content"),
            small(year)
        )
    }
)

# Render templates by calling them

page_main()

#> <div>
#>  <h1>Dashboard</h1>
#>  <p>Welcome back</p>
#>   <small>2026</small>
#> </div>

# Extract fragments from templates

page_main(fragment="content")

#> <p>Welcome back</p>
```

------------------------------------------------------------------------

1.  not yet implemented

2.  differs to base plumber2 implementation in that we render the entire
    tag tree, allowing for emitting head tags amongst others.
