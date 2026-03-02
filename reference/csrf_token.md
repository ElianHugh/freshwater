# CSRF Token

`csrf_token()` returns the current CSRF token string for the active
request when used within a
[`template()`](https://elianhugh.github.io/freshwater/reference/templating.md).
Calling it outside of a
[`template()`](https://elianhugh.github.io/freshwater/reference/templating.md)
context will result in an error.

In most cases, CSRF tokens are inserted automatically for standard form
helpers. Intended for custom forms / custom token placement (meta tags,
JS fetch, etc).

## Usage

``` r
csrf_token()
```

## See also

[api_csrf](https://elianhugh.github.io/freshwater/reference/api_csrf.md)

## Examples

``` r
page <- template({
    html(
        head(
            meta(name = "csrf-token", content = csrf_token())
        ),
        body(
            div("App content")
        )
    )
})
page()
#> <html>
#>   <body>
#>     <div>App content</div>
#>   </body>
#> </html>
```
