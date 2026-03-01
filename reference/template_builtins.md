# Template Built-ins

The following helpers are automatically injected inside
[`template()`](https://elianhugh.github.io/freshwater/reference/templating.md)
bodies.

### form(...)

Wraps `htmltools::tags$form()`.

- If CSRF middleware is active, a hidden `csrf_token` input is
  automatically injected.

- If `method` is one of "put", "patch", or "delete", a hidden `_method`
  input is added and the HTML form method is set to "post".

  - Browsers only support GET and POST. When method is "put", "patch",
    or "delete", freshwater renders a POST form with a hidden \_method
    field. Middleware interprets this as the effective HTTP method.
    Requires context-enabled middleware.

### csrf_token()

- Returns the current CSRF token string for the active request

- Intended for custom forms / custom token placement (meta tags, JS
  fetch, etc).
