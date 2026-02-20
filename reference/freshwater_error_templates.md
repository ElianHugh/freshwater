# Error Page Templates

freshwater provides a number of default views that are served to HTML
clients in the event of common HTTP error codes.

## Usage

``` r
default_error_500_template(
  error = NULL,
  request = NULL,
  is_debug = FALSE,
  ...,
  fragment = NULL
)

default_error_404_template(error = NULL, request = NULL, ..., fragment = NULL)

default_error_403_template(error = NULL, request = NULL, ..., fragment = NULL)
```

## Arguments

- error:

  the error condition signaled by an error in the server's route handler

- request:

  the
  [reqres::Request](https://reqres.data-imaginist.com/reference/Request.html)
  request object the handler is responding to

- is_debug:

  whether to provide the stack trace and error message to the web
  client. Although useful during development, it is heavily recommended
  to set as `FALSE` in production as it can leak sensitive information.

- ...:

  unused

- fragment:

  unused

## See also

[api_error_pages](https://elianhugh.github.io/freshwater/reference/error_pages.md),
[template](https://elianhugh.github.io/freshwater/reference/templating.md)
