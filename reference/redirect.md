# Redirect to another resource

Sends a 303 response and halts request processing. Client is redirected
to the given location.

## Usage

``` r
redirect(response, location)
```

## Arguments

- response:

  Plumber2 response object

- location:

  path or url to redirect to

## Details

This is commonly used in Post/Redirect/GET (PRG) setups to redirect
clients to a new page following form submissions.

## Examples

``` r
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
#> <environment: 0x55b09a03cd40>
```
