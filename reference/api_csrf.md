# Apply CSRF Protection to a plumber2 API

Installs CSRF middleware on a plumber2 API using the double-submit
cookie pattern.

## Usage

``` r
api_csrf(api, secure = TRUE)
```

## Arguments

- api:

  a plumber2 API object

- secure:

  if `TRUE`, sets the CSRF cookie to "\_\_Host-csrf" and marks the
  cookie as secure. If false, uses "csrf".

## Details

When installed:

- Any `form` element inside
  [template](https://elianhugh.github.io/freshwater/reference/templating.md)
  automatically include a CSRF token.

- If working in JavaScript contexts, the `csrf_token()` helper is also
  accessible inside templates.

Middleware behaviour:

- On **safe** methods (`GET`, `HEAD`, `OPTIONS`), if the CSRF cookie is
  missing, a new token is generated and set as a cookie.

- On **unsafe** methods (`POST`, `PUT`, `DELETE`, `PATCH`), the request
  is rejected with **403** unless a token provided via the
  `X-CSRF-Token` header or a `csrf_token` field in the parsed request
  body matches the CSRF cookie.

## Examples

``` r
#* @plumber
function(api) {
  api |>
       api_csrf(secure = FALSE)
}
#> function (api) 
#> {
#>     api_csrf(api, secure = FALSE)
#> }
#> <environment: 0x55d0436299f0>
```
