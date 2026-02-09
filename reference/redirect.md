# Redirect to another resource

Sends a 303 response and halts request processing.

## Usage

``` r
redirect(response, location)
```

## Arguments

- response:

  Plumber2 response object

- location:

  path or url to redirect to

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
#> <environment: 0x55ba83218f90>
```
