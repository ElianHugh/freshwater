# Freshwater Error Pages

Adds request/error hooks to a
[plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
API so that freshwater can render friendly **HTML error pages** for:

- **403 Forbidden** responses

- **404 Not Found** responses

- **500 Internal Server Error** conditions

## Usage

``` r
api_error_pages(
  api,
  handlers = NULL,
  debug = plumber2::get_opts("fw_debug", default = interactive())
)
```

## Arguments

- api:

  a
  [plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
  api object.

- handlers:

  optional list of named error templates. Supported keys are: "403",
  "404", "500". If omitted, freshwater installs default templates.

- debug:

  whether the **500** error template should render error messages and
  stack traces. Defaults to the `fw_debug` plumber2 option, and falls
  back to [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Details

Custom error page templates can be supplied via the `handler` parameter.
These should be freshwater templates created via
[`template()`](https://elianhugh.github.io/freshwater/reference/templating.md),
and should match the call signatures of the default error templates. See
[freshwater_error_templates](https://elianhugh.github.io/freshwater/reference/freshwater_error_templates.md)
for the relevant template signatures required.

## See also

[freshwater_error_templates](https://elianhugh.github.io/freshwater/reference/freshwater_error_templates.md),
[enhook_routes](https://elianhugh.github.io/freshwater/reference/hooks.md)

## Examples

``` r
#* @plumber
function(api) {
 api |>
     api_error_pages(debug = TRUE)
}
#> function (api) 
#> {
#>     api_error_pages(api, debug = TRUE)
#> }
#> <environment: 0x55d041e398e0>
```
