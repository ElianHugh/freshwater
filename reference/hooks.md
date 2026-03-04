# Route handler hooks

Add middleware-style hooks to *all existing user handlers* in a
[plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
API. Hooks execute in a deterministic order prior to the user handler,
and can mutate/intercept requests and responses, as well as
short-circuit handlers. This allows for middleware-type behaviour
without registering additional routes.

## Usage

``` r
api_hooks(api, hooks, .where = c("append", "prepend"))

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
  arguments. If a hook returns without calling `next_call()`, the
  remaining hooks and user handler are skipped, and the return value
  becomes the handler result.

- .where:

  whether the hooks should be appended or prepended to the list of
  installed hooks

- id:

  id of the hook

- fn:

  function with signature `fn(api, args, next_call)`

## Details

System hooks are installed in the following order:

- `freshwater::context`

- `freshwater::error_pages`

- `freshwater::csrf`

- `freshwater::csrf_context`

### Control Flow

Hooks control whether subsequent hooks should execute. Concretely:

- To continue to the next hook (and eventual user handler), call
  `next_call()`.

- To short-circuit the chain, return a value without calling
  `next_call()`.

- To bubble up to plumber2 routing control flow, return either
  [`plumber2::Next`](https://plumber2.posit.co/reference/Next.html) or
  [`plumber2::Break`](https://plumber2.posit.co/reference/Next.html)
  (and don't call `next_call()`).

Hooks can also wrap later hooks and the user handler by calling
`next_call()`, and then doing work after.

### Hook Installation

- Routing is managed by
  [plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
  – this function does not change routing precedence. Within the handler
  itself, however, hooks run in the order they are installed.

- The function is idempotent (with respect to either a computed hash of
  the hook or a provided id), and only new hooks will be installed.

### Asynchronous Routes

When using asynchronous routes via `async=TRUE` programmatically, or via
`@async`, hooks are attached to the `then` handlers, rather than main
handler itself. This is because `request`, `response`, and `server`
arguments are not available to the main async handler, and hooks depend
on the full handler signature being available.

## Examples

``` r
api <- plumber2::api() |>
  plumber2::api_get(path = "/", function() {
    "Foo"
  })
#> Creating default route in request router

log_hook <- hook(
  "logger",
  function(api, args, next_call) {
    msg <- sprintf(
      "[%s] %s %s",
      format(Sys.time(), "%H:%M:%S"),
      args$request$method,
      args$request$path
    )
    print(msg)
    next_call()
  }
)

timer_hook <- hook("timer", function(api, args, next_call) {
  t0 <- Sys.time()
  out <- next_call()
  print(sprintf("time: %s", Sys.time() - t0))
  out
})
api <- api_hooks(api, hooks = list(log_hook, timer_hook))
```
