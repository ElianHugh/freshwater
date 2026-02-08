# Clear the cache of all memoised templates

Clear the cache of all memoised templates

## Usage

``` r
cache(name, vary = NULL, ...)

clear_cache()

invalidate_cache(tpl, name, vary = NULL, fragment = NULL)

invalidate_cache_here(name, vary = NULL, fragment = NULL)
```

## Arguments

- name:

  unique name for the cached partial template

- vary:

  values that should change when the cached output should change. This
  is used to construct the cache key.

- ...:

  tag content to render and cache

- tpl:

  a template function created by
  [`template()`](https://elianhugh.github.io/freshwater/reference/templating.md).

- fragment:

  optional fragment name for targetting cached fragments

## See also

[template](https://elianhugh.github.io/freshwater/reference/templating.md)

## Examples

``` r
# Caching
nav <- template(user, {
  div(
    cache(
      "nav",
      vary = user$id,
      ul(
        li("Home"),
        li("Profile"),
        if (user$is_admin) li("Admin")
      )
    )
  )
})
nav(list(id = 1, is_admin = TRUE))
#> <div><ul>
#>   <li>Home</li>
#>   <li>Profile</li>
#>   <li>Admin</li>
#> </ul></div>

# Nested Caches
dashboard <- template(page = list(), stats = list(), recent = list(), {
    cache(
        name = "page",
        vary = page$updated_at,
        div(
            h1("Dashboard"),
            cache(
                name = "stats",
                 vary = stats$updated_at,
                div(p(stats$count))
            ),
            cache(
                name = "recent",
                vary = recent$updated_at,
                div(recent)
            )
        )
    )
})
dashboard()
#> <div>
#>   <h1>Dashboard</h1>
#>   <div>
#>   <p></p>
#> </div>
#>   <div></div>
#> </div>

# Invalidate the current cache
# during rendering

page <- template(user, {
  div(
    cache(
      name = "content",
      vary = user$id,
      {
        if (user$refresh) {
          invalidate_cache_here(
            name = "content",
            vary = user$id
          )
        }
        p("Hello ", user$id)
      }
    )
  )
})

page(list(id = 1, refresh = FALSE))
#> <div><p>
#>   Hello 
#>   1
#> </p></div>

page(list(id = 1, refresh = TRUE))
#> <div><p>
#>   Hello 
#>   1
#> </p></div>
```
