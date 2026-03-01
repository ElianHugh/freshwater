# Freshwater Request Context

freshwater installs a per-request execution context that allows
[`current_path()`](https://elianhugh.github.io/freshwater/reference/current_path.md),
csrf_token(), and
[`template()`](https://elianhugh.github.io/freshwater/reference/templating.md)
helpers to access the active HTTP request. The context itself is stored
in freshwater's internal state and is set/unset with each request.

Context is created automatically when
[`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md),
[`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
or
[`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md)
is installed.

Method spoofing is applied during the `before-request` phase by
rewriting `REQUEST_METHOD` when a hidden `_method` field is present.

## Lifecycle

The context exists only during an active HTTP request. Calling
context-dependent helpers outside a request will raise a
`freshwater_context_missing` error.

## See also

[`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md),
[`current_path()`](https://elianhugh.github.io/freshwater/reference/current_path.md)
