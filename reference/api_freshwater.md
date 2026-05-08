# Freshwater defaults for plumber2 APIs

Installs freshwater defaults onto a plumber2 API.

This is a convenience wrapper:

- Registers freshwater's HTML serialiser

- Registers freshwater's async evaluator

- Installs freshwater request context

- Optionally enables CSRF protection

- Optionally installs HTML error page handlers

Arguments in `...` are selectively forwarded to
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md),
and
[register_html_serialiser](https://elianhugh.github.io/freshwater/reference/register_html_serialiser.md)
based on matching formal parameters.

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

  args passed to
  [`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
  [`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md),
  [`register_html_serialiser()`](https://elianhugh.github.io/freshwater/reference/register_html_serialiser.md),
  or
  [`register_async_evaluator()`](https://elianhugh.github.io/freshwater/reference/register_async_evaluator.md)

## See also

[api_csrf](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[api_error_pages](https://elianhugh.github.io/freshwater/reference/error_pages.md),
[register_html_serialiser](https://elianhugh.github.io/freshwater/reference/register_html_serialiser.md),
[register_async_evaluator](https://elianhugh.github.io/freshwater/reference/register_async_evaluator.md)
