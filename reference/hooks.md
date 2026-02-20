# Route handler hooks

Add middleware-style hooks to *all existing routes* in a
[plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
API.

## Usage

``` r
enhook_routes(api, hooks, .where = c("append", "prepend"))

hook(id = NULL, fn)
```

## Arguments

- api:

  a
  [plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
  api object.

- hooks:

  a single hook or list of hooks that take the signature
  `fn(api, args, next_call)`, where `args` is the list of handler
  arguments. The return value of the hook should be `next_call()` to
  facilitate calling of subsequent hooks & the user handler function.
  Not calling next_call() will short-circuit the handler chain.

- .where:

  whether the hooks should be appended or prepended to the list of
  installed hooks

- id:

  id of the hook

- fn:

  function with signature `fn(api, args, next_call)`

## Details

Hooks are called iteratively in order of installation (barring if they
are prepended), culminating in the final user handler.

- Routing is managed by
  [plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
  – this function does not change routing precedence. Within the handler
  itself, however, hooks run in the order they are installed.

- The function is idempotent (with respect to either a computed hash of
  the hook or a provided id), and only new hooks will be installed.
