# Form

When used within a
[`template()`](https://elianhugh.github.io/freshwater/reference/templating.md),
a form implementation is injected that wraps `htmltools::tags$form()`.

## Usage

``` r
form(..., method = "get")
```

## Arguments

- ...:

  tag attributes and children passed to the `htmltools::tags$form()`
  function

- method:

  character scalar denoting the HTTP method to perform. One of:

  - "get"

  - "post"

  - "put"

  - "patch"

  - "delete"

## Value

(When injected) An
[`htmltools::tag`](https://rstudio.github.io/htmltools/reference/builder.html)
object.

## Details

When a request context is available, freshwater adds optional behaviors
such as CSRF token insertion and HTTP method spoofing.

Calling `form()` outside of
[`template()`](https://elianhugh.github.io/freshwater/reference/templating.md)
rendering will result in an error. For a plain form tag in normal R
code, use
[htmltools::tags](https://rstudio.github.io/htmltools/reference/builder.html)`$form()`.

### CSRF

- If CSRF middleware is active, a hidden `csrf_token` input is
  automatically injected.

### Method Spoofing

If `method` is one of "put", "patch", or "delete", a hidden `_method`
input is added and the HTML form method is set to "post".

Browsers only support GET and POST. When method is "put", "patch", or
"delete", freshwater renders a POST form with a hidden \_method field.
Middleware interprets this as the effective HTTP method. Requires
freshwater context-enabled middleware.

## See also

[template](https://elianhugh.github.io/freshwater/reference/templating.md),
[api_csrf](https://elianhugh.github.io/freshwater/reference/api_csrf.md),
[api_freshwater](https://elianhugh.github.io/freshwater/reference/api_freshwater.md),
[htmltools::tags](https://rstudio.github.io/htmltools/reference/builder.html)

## Examples

``` r
page <- template({
     form(method = "delete")
})
page()
#> <form method="post">
#>   <input type="hidden" name="_method" value="DELETE"/>
#> </form>
```
