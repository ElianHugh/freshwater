# Get request data from current context

These read-only helpers provide access to request data for the current
HTTP request via the freshwater request context.

- `current_path()` returns the request URL path

- `current_method()` returns the HTTP method

- `current_query()` returns the query parameters

- `current_cookie()` returns the value of a cookie by name

- `current_header()` returns the value of a header by name

These functions are primarily intended for use inside templates where a
request context has been established. If called outside of an active
context, an error is raised.

Context is available when freshwater context middleware is active
(installed automatically by
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md),
or
[`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md)).

## Usage

``` r
current_path()

current_method()

current_query()

current_cookie(name)

current_header(name)
```

## Arguments

- name:

  the name of a cookie or header

## See also

[`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md),
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md)
