# Create a reusable HTML template

`template()` is a function factory that captures a template expression
and returns a callable HTML renderer. The expression is evaluated under
the
[`htmltools::withTags()`](https://rstudio.github.io/htmltools/reference/withTags.html)
environment, so tag functions such as `div()` or `p()` are available.

Templates may define:

- **parameters**: symbols or named defaults which are used as arguments
  to the renderer

- **content injection**: if the template uses `...`, the renderer passes
  `...` to the containing HTML nodes defined in the template.

- **fragments**: named subtemplates that can be optionally extracted
  from the template upon rendering by supplying `fragment = "name"`.
  Fragment names are required.

## Usage

``` r
template(..., .envir = parent.frame())

fragment(..., name = NULL)

cache(name, vary = NULL, ...)

clear_cache(reset = TRUE)
```

## Arguments

- ...:

  tag content to render and cache

- .envir:

  the environment in which to evaluate the template

- name:

  unique name for the cached partial template

- vary:

  values that should change when the cached output should change. This
  is used to construct the cache key.

## Value

function of class `template` with interface
`fn(<declared params>, ..., fragment = NULL)`

## Attributes

Attributes with non-leading underscores are rewritten as hyphenated
versions instead. This means you can write
`htmltools::div(data_foo="bar")` which is converted to
`htmltools::div(data-foo="bar")`.

An escape hatch exists If you explicitly want underscores in your
attributes. You may use double underscores, which will be converted to
single underscores e.g. `htmltools::div(data__foo="bar")` which is
converted to `htmltools::div(data_foo="bar")`.

## Examples

``` r
# Example Fragment Usage
page_main <- template(
    {
        div(
            h1("Dashboard"),
            fragment(p("Welcome back"), name = "content"),
            small("2026")
        )
    }
)

page_main(fragment="content")
#> <p>Welcome back</p>

# Template slots

details <- template(name, age, {
    nm <- sprintf("Hello, my name is: %s", name)
    old <- sprintf("I am %s years old.", age)
    div(
        p(nm),
        p(old)
    )
})

details("Jim", 30)
#> Error in rewrite_attrs(tag): could not find function "rewrite_attrs"

# Templates and fragments can also be combined

card <- template(
    ttl, foot = NULL, {
        div(
            h2(ttl),
            fragment(div("Card body"), name="body"),
            if (!is.null(foot)) {
                fragment(
                    div(foot),
                    name = "footer"
                )
            }
        )
    }
)
card("Card Title")
#> Error in rewrite_attrs(tag): could not find function "rewrite_attrs"
card("Card Title", fragment="body")
#> <div>Card body</div>
card("Card Title", "Footer text", fragment = "footer")
#> <div>Footer text</div>

# Dots (content injection)
layout <- template({
    htmltools::tagList(
        head(meta(title = "foo")),
        body(...)
    )
})

layout(htmltools::div("content"))
#> Error in as.character(x): cannot coerce type 'closure' to vector of type 'character'

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
#> Error in rewrite_attrs(tag): could not find function "rewrite_attrs"

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
```
