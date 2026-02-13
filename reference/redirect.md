# Redirect to another resource

If `after` is NULL, sends a 303 response and halts request processing.
Client is redirected to the given location. This is commonly used in
Post/Redirect/GET (PRG) setups to redirect clients to a new page
following form submissions.

## Usage

``` r
redirect(response, location, after = NULL)
```

## Arguments

- response:

  Plumber2 response object

- location:

  path or url to redirect to

- after:

  number of seconds to wait before redirection

## Value

- Plumber2::Break when issuing an immediate redirect.

- Plumber2::Next when issuing a delayed navigation.

## Details

If after is a numeric, a "Refresh" header is attached to the response,
instructing the browser to navigate to `location` after the specified
number of seconds.

The delayed redirect uses the non-standard "Refresh" HTTP header which
is widely supported by browsers but is not part of the official HTTP
specification. It should not be relied on for API & non-browser clients.

## Examples

``` r
# Immediate redirect (PRG pattern)
#* @get /
function(response) {
    print("Hello!")
    redirect(response, "/foo")
}
#> function (response) 
#> {
#>     print("Hello!")
#>     redirect(response, "/foo")
#> }
#> <environment: 0x55c4e0250ca8>

# Delayed redirect after rendering content
#* @get /count/<n>
function(n, response) {
  redirect(response, "/", after = 1)
  paste("n =", n)
}
#> function (n, response) 
#> {
#>     redirect(response, "/", after = 1)
#>     paste("n =", n)
#> }
#> <environment: 0x55c4e0250ca8>
```
