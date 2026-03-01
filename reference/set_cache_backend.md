# Configure freshwater's cache backend

`set_cache_backend()` replaces the cache backend used by
[`cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md).

## Usage

``` r
set_cache_backend(backend)
```

## Arguments

- backend:

  cache backend accepted by
  [memoise::memoise](https://memoise.r-lib.org/reference/memoise.html)

## Details

This function allows controlling cache persistence (memory vs disk),
eviction policies, and storage limits via the backend object.

## See also

[`cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md),
[`clear_cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md),
[memoise::memoise](https://memoise.r-lib.org/reference/memoise.html)
