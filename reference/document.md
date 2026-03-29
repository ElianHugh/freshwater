# HTML Document Root

Constructs a full HTML document. It does not modify or validate its
contents.

`document()` is used for full-page responses, and should not be used for
partials, fragments or nested templates.

## Usage

``` r
document(...)
```

## Arguments

- ...:

  user-supplied content

## Value

An
[`htmltools::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html),
consisting of a doctype declaration, an tag, and user-supplied content.

## See also

[template](https://elianhugh.github.io/freshwater/reference/templating.md),
[fragment](https://elianhugh.github.io/freshwater/reference/templating.md)

## Examples

``` r
document(
     htmltools::tags$head(
         htmltools::tags$title("Home")
     ),
     htmltools::tags$body(
         htmltools::tags$h1("Hello")
     )
)
#> <!DOCTYPE html>
#> <html>
#>   <body>
#>     <h1>Hello</h1>
#>   </body>
#> </html>
```
