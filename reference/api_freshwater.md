# Freshwater defaults for plumber2 APIs

This is a convenience wrapper around
[`register_html_serialiser()`](https://elianhugh.github.io/freshwater/reference/register_html_serialiser.md),
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
and
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md).

This middleware installs freshwater request context.

## Usage

``` r
api_freshwater(api, csrf = TRUE, error_pages = TRUE, ...)
```

## Arguments

- api:

  a
  [plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
  api object.

- csrf:

  whether to enable CSRF protection

- error_pages:

  whether to enable error pages

- ...:

  args passed to either
  [`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md)
  or
  [`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md)
