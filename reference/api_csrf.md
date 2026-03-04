# Apply CSRF Protection to a plumber2 API

`api_csrf()` installs CSRF middleware on a plumber2 API using the
double-submit cookie pattern.

## Usage

``` r
api_csrf(api, secure = TRUE, exemptions = character())
```

## Arguments

- api:

  a plumber2 API object

- secure:

  if `TRUE`, sets the CSRF cookie to "\_\_Host-csrf" and marks the
  cookie as secure. If false, uses "csrf".

- exemptions:

  character vector of route patterns to exempt from CSRF checks

## Details

When installed:

- Any `form` element inside
  [template](https://elianhugh.github.io/freshwater/reference/templating.md)
  automatically includes a CSRF token.

- If working in JavaScript contexts, the
  [`csrf_token()`](https://elianhugh.github.io/freshwater/reference/csrf_token.md)
  helper is also accessible inside templates.

Middleware behaviour:

- On **safe** methods (`GET`, `HEAD`, `OPTIONS`), if the CSRF cookie is
  missing, a new token is generated and set as a cookie.

- On **unsafe** methods (`POST`, `PUT`, `DELETE`, `PATCH`), the request
  is rejected with **403 Forbidden** unless a token provided via the
  `X-CSRF-Token` header or a `csrf_token` field in the parsed request
  body matches the CSRF cookie.

This middleware installs freshwater request context.

## Annotation Reference

CSRF exemptions can be specified by `@csrf`:

- `"on"`: (default) CSRF checks are enforced

- `"off"` or `"exempt"`: CSRF checks are skipped for the route

    #* @post /foo/*/bar
    #* @csrf exempt
    function() {
     print("No checking!")
    }

## See also

[form](https://elianhugh.github.io/freshwater/reference/form.md),
[api_freshwater](https://elianhugh.github.io/freshwater/reference/api_freshwater.md),
[api_hooks](https://elianhugh.github.io/freshwater/reference/hooks.md)

## Examples

``` r
#* @plumber
function(api) {
  api |>
       api_csrf(secure = FALSE, exemptions = c("/foo/*", "/bar"))
}
#> function (api) 
#> {
#>     api_csrf(api, secure = FALSE, exemptions = c("/foo/*", "/bar"))
#> }
#> <environment: 0x55b24c028a68>

```
