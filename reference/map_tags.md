# Apply template function to each element of a vector

`map_tags()` returns a type-safe tag list, where each element is
resolved by applying `.f` to each element of `.x`. If an element in `.x`
is a falsey value (i.e. `NA`, `NaN`, `FALSE`, or `NULL`), the fallback
value from `.empty` is used.

Additional arguments should be passed with an anonymous function.

## Usage

``` r
map_tags(.x, .f, .empty = NULL)
```

## Arguments

- .x:

  list or atomic vector

- .f:

  a function that takes a single argument returns a character vector,
  tag, or tagList.

- .empty:

  fallback value for falsey elements

## Details

Element values are evaluated prior to returning the tag list:

- NULL values are removed from the final tag list. The return length of
  `map_tags()` is therefore less than or equal to the length of `.x`

- All return values must be either a "shiny.tag", "shiny.tag.list", or
  "character" vector. An error is raised if an unexpected value is
  encountered.

## See also

[template](https://elianhugh.github.io/freshwater/reference/templating.md),
[base::lapply](https://rdrr.io/r/base/lapply.html),
[htmltools::tagList](https://rstudio.github.io/htmltools/reference/tagList.html)

## Examples

``` r
tpl <- template(x, {p(x)})
map_tags(seq(5L), tpl)
#> <p>1</p>
#> <p>2</p>
#> <p>3</p>
#> <p>4</p>
#> <p>5</p>

# falsey values are removed
map_tags(c(TRUE, FALSE, TRUE), tpl)
#> <p>TRUE</p>
#> <p>TRUE</p>
```
