# Get current request path from context

Return the URL path of the current HTTP request, as captured in the
freshwater request context. If called outside of an active context, an
error is raised.

This is primarily intended for use inside templates where the request
context has been established.

Context is available when freshwater context middleware is active
(installed automatically by
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md),
or
[`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md)).

## Usage

``` r
current_path()
```

## See also

[`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md),
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md)
