# Get current request path from context

Return the URL path of the current HTTP request, as captured in the
freshwater request context. If called outside of an active context, an
empty string is returned.

This is primarily intended for use inside templates where the request
context has been established.

Context is Available when freshwater context middleware is active
(installed automatically by
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md),
or \[api_freshwater())\].

\[api_freshwater())\]: R:api_freshwater())

## Usage

``` r
current_path()
```

## See also

[`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md),
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md)
