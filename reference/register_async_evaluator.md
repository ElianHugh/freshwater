# Register context-safe async evaluator

Registers an asynchronous evaluator for routes, allowing freshwater
context to be propagated to
[mirai::mirai](https://mirai.r-lib.org/reference/mirai.html) workers.
This means that contextful helpers such as
[current_method](https://elianhugh.github.io/freshwater/reference/current_context.md),
[current_path](https://elianhugh.github.io/freshwater/reference/current_context.md),
and
[current_query](https://elianhugh.github.io/freshwater/reference/current_context.md)
(among others) will work in async routes.

## Usage

``` r
register_async_evaluator(force = FALSE, set_default = TRUE)
```

## Arguments

- force:

  whether to register the evaluator regardless of if it has been
  registered already

- set_default:

  whether to set the default async evaluator to `"freshwater"`

## Details

Context is not inherently portable across asynchronous contexts, this
function creates a portable snapshot of the current context that is
passed to a mirai worker.

Hooks are *not* applied to the async route, but may be provided to any
associated `then` handlers. If error pages are installed on the main
process, errors from the worker will be appropriately converted into
freshwater error pages.

Requires the
[promises::promises](https://rstudio.github.io/promises/reference/promises-package.html),
[mirai::mirai](https://mirai.r-lib.org/reference/mirai.html), and
[mori::mori](https://shikokuchuo.net/mori/reference/mori-package.html)
packages.

## See also

[api_freshwater](https://elianhugh.github.io/freshwater/reference/api_freshwater.md),
[api_error_pages](https://elianhugh.github.io/freshwater/reference/error_pages.md),
[api_hooks](https://elianhugh.github.io/freshwater/reference/hooks.md),
[mirai::mirai](https://mirai.r-lib.org/reference/mirai.html),
[current_method](https://elianhugh.github.io/freshwater/reference/current_context.md)

## Examples

``` r
register_async_evaluator()
#* @async
#* @get /async
function() {
 current_path()
}
#> function () 
#> {
#>     current_path()
#> }
#> <environment: 0x558ab4aa2940>
```
