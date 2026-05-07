# Combine multiple target selectors

`targets()` returns a comma-separated string combining multiple
[`target()`](https://elianhugh.github.io/freshwater/reference/target.md)
calls. If a template is supplied,
[`target()`](https://elianhugh.github.io/freshwater/reference/target.md)
is called on it, otherwise the value is coerced to character and used
as-is.

## Usage

``` r
targets(...)
```

## Arguments

- ...:

  templates or character vectors

## See also

[target](https://elianhugh.github.io/freshwater/reference/target.md)

## Examples

``` r
tpl <- template(.id = "foo", {})
tpl2 <- template(x, .id = function(x) x, {})
targets(
    tpl,
   target(tpl2, x = "bar")
)
#> [1] "[id=\"foo\"], [id=\"bar\"]"
```
