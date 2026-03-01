# Get freshwater's current cache backend

Returns the cache backend currently used by
[`cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md).

## Usage

``` r
get_cache_backend()
```

## Value

A cache backend object (typically from `cachem`), or `NULL` if the cache
has not yet been initialised.

## See also

[`set_cache_backend()`](https://elianhugh.github.io/freshwater/reference/set_cache_backend.md),
[`cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)

## Examples

``` r
get_cache_backend()
#> <cache_mem> <cachem>
#>   Methods:
#>     get(key, missing = missing_)
#>     set(key, value)
#>     exists(key)
#>     keys()
#>     remove(key)
#>     reset()
#>     prune()
#>     size()
#>     info()
```
