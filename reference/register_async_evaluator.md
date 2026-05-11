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
register_async_evaluator(set_default = TRUE)
```

## Arguments

- set_default:

  whether to set the default async evaluator to `"freshwater"`

## Details

**Registration affects global plumber2 state, not just the current API
process.**

Context is not inherently portable across asynchronous request contexts,
this function creates a portable snapshot of the current context that is
passed to a mirai worker.

Hooks are *not* applied to the async route, but may be provided to any
associated `then` handlers. If error pages are installed on the main
process, errors from the worker will be appropriately converted into
freshwater error pages. If CSRF protection is enabled, tokens will be
propagated to the worker, ensuring async routes are still protected.

As
[`cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
is process-local by default, memoised functions are *not* ported to
workers. Likewise,
[`clear_cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
and
[`invalidate_cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
will only impact the local process' cache. If a shared cache is desired,
consider configuring
[`cachem::cache_disk()`](https://cachem.r-lib.org/reference/cache_disk.html)
for caching, which will allow all process to utilise a shared cache.
Note that TTL is process-local regardless of backend strategy used.

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
#> <environment: 0x55b17559c258>
```
