
# 🌿💧 freshwater

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![freshwater status badge](https://elianhugh.r-universe.dev/badges/freshwater)](https://elianhugh.r-universe.dev/freshwater)
[![Codecov test coverage](https://codecov.io/gh/ElianHugh/freshwater/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ElianHugh/freshwater?branch=main)
[![R-CMD-check](https://github.com/ElianHugh/freshwater/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ElianHugh/freshwater/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

> [!NOTE]
> This package is under active development and its functionality may change over time.

freshwater provides server-side rendering utilities for plumber2 backends:

- composable HTML templates (slots, parameters, and fragments)
- template caching
- weak ETag caching
- shiny tag serialisation[^1]
- CSRF protection
- ... and more!

[^1]: differs to base plumber2 implementation in that we render the entire tag tree, allowing for emitting head tags amongst others.

For autoreloading support, consider [hotwater](https://github.com/ElianHugh/hotwater).

## Installation

You can install the development version of freshwater from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ElianHugh/freshwater")
```

## Reusable templates

```r
library(freshwater)

details <- template(name, age, {
  div(
    p(sprintf("Name: %s", name)),
    p(sprintf("Age: %s", age))
  )
})

details("Jim", 30)
```

```html
<div>
    <p>Name: Jim</p>
    <p>Age: 30</p>
</div>
```

## Fragments

``` r
card <- template(title, {
  div(
    h2(title),
    fragment(
        div("Card body"),
        name = "body"
    ),
    fragment(
        div("Footer"),
        name = "footer"
    )
  )
})

card("Hello")
```

```html
<div>
  <h2>Hello</h2>
  <div>Card body</div>
  <div>Footer</div>
</div>
```

```r
card("Hello", fragment = "body")
```

```html
<div>Card body</div>
```

## Layouts & content injection

```r
layout <- template({
    div(
    head(title("App")),
    body(...)
  )
})

layout(
  htmltools::div(
    htmltools::h1("Dashboard"),
    htmltools::p("Welcome back")
  )
)
```

```html
<div>
  <body>
    <div>
      <h1>Dashboard</h1>
      <p>Welcome back</p>
    </div>
  </body>
</div>

```

## Attribute name normalisation

> Attribute names with non-leading underscores are rewritten to hyphenated HTML attributes
> Double underscores (__) act as an escape hatch for literal underscores

```r
template({
  div(
    hx_get = "/items",
    hx_target = "#main",
    data_user_id = 42,
    `data__raw__name` = "keep_underscore",
    "Load"
  )
})()
```

```html
<div
    hx-get="/items"
    hx-target="#main"
    data-user-id="42"
    data_raw_name="keep_underscore"
>
    Load
</div>
```

## Cached partials

> Cached partials are keyed by template, fragment, cache name, and vary.

```r
nav <- template(user, {
  ul(
    cache(
      name = "nav",
      vary = user$id,
      li("Home"),
      li("Profile"),
      if (user$is_admin) li("Admin")
    )
  )
})

nav(list(id = 1, is_admin = TRUE))
```

```html
<ul><li>Home</li>
<li>Profile</li>
<li>Admin</li></ul>
```

## Nested Caches

```r
dashboard <- template(page, stats, {
  cache(
    "page",
    vary = page$updated_at,
    div(
      h1("Dashboard"),
      cache(
        "stats",
        vary = stats$updated_at,
        p(stats$count)
      )
    )
  )
})

dashboard(
  page  = list(updated_at = 1),
  stats = list(updated_at = 2, count = 42)
)

dashboard(
  page  = list(updated_at = 1),
  stats = list(updated_at = 2, count = 42)
)
```

```html
<div>
  <h1>Dashboard</h1>
  <p>42</p>
</div>

[cached partial]
<div>
  <h1>Dashboard</h1>
  <p>42</p>
</div>

```

## Conditional GETs

> If the client’s If-None-Match header matches the current ETag, freshwater returns 304 Not Modified and skips rendering.

```r
#* @get /dashboard
#* @etag \() as.integer(Sys.Date())
function() {
  page_main()
}
```

## CSRF Protection

> Prevent hijacking unsafe HTTP methods via the double-submit cookie pattern

```r
function(api) {
  api |>
    api_csrf(secure = TRUE)
}
```

## Error Pages

> Provide pretty error-pags for common HTTP error codes (403, 404, and 500)

```r
function(api) {
  api |>
    api_error_pages(debug=TRUE)
}
