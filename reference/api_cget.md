# Conditional GET

Creates a conditional GET handler for a specific HTTP path, using a
supplied etag function for the current representation. If the server's
ETag and client's 'If-None-Match' headers match, a `304 Not Modified`
response is sent, short-circuiting downstream handlers. This reduces the
need to recompute responses for paths where the data has not changed
since the last client request.

## Usage

``` r
api_cget(api, path, etag_fn)
```

## Arguments

- api:

  a
  [plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
  api object.

- path:

  the path to short circuit.

- etag_fn:

  a function that takes no arguments and returns a single value used to
  derive the ETag.

## Details

See
<https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Conditional_requests>
for more information.

## Annotation Reference

The etag function is specified by `@etag fn` where `fn` is the function
name. Functions can also be defined in-line like `@etag \() x + 1`.

    increment_x <- \() {
        x <<- x + 1L
        later::later(increment_x, delay = 10L)
    }
    x <- 1L
    increment_x()
    #* @get /
    #* @etag \() x
    function() {
     x
    }
