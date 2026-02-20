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
template(..., .envir = rlang::caller_env())

fragment(name = NULL, ...)
```

## Arguments

- ...:

  template definition. Provide zero or more parameters, followed by a
  single braced expression.

- .envir:

  the environment in which to evaluate the template

- name:

  the name of the fragment

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

## See also

[cache](https://elianhugh.github.io/freshwater/reference/template-caching.md)

## Examples

``` r
# Example Fragment Usage
page_main <- template(
    {
        div(
            h1("Dashboard"),
            fragment(
                 p("Welcome back"),
                 name = "content"
            ),
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
#> <div>
#>   <p>Hello, my name is: Jim</p>
#>   <p>I am 30 years old.</p>
#> </div>

# Templates and fragments can also be combined

card <- template(
    ttl, footer = NULL, {
        div(
            h2(ttl),
            fragment(div("Card body"), name="body"),
            if (!is.null(footer)) {
                fragment(
                    div(footer),
                    name = "footer"
                )
            }
        )
    }
)
card("Card Title")
#> <div>
#>   <h2>Card Title</h2>
#>   <div>Card body</div>
#> </div>
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
#> <body>
#>   <div>content</div>
#> </body>
```
