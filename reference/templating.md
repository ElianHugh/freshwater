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
  Fragment names are required. If multiple fragment names are specified,
  fragments will be extracted and collated into a
  [htmltools::tagList](https://rstudio.github.io/htmltools/reference/tagList.html),
  in the order of names provided. If a specified fragment cannot be
  found, an error will be raised.

## Usage

``` r
template(..., .id = NULL, .envir = rlang::caller_env())

fragment(name = NULL, ...)
```

## Arguments

- ...:

  template definition. Provide zero or more parameters, followed by a
  single braced expression.

- .id:

  a character scalar or function that returns a character scalar. The
  result is provided as an id attribute to the root of the template.

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

Attributes with trailing underscores have their underscores stripped.
This means the you can write `htmltools::tags$label(for_="foo")` which
is converted to `htmltools::tags$label(for="foo")`.

An escape hatch exists If you explicitly want underscores in your
attributes. You may use double underscores, which will be converted to
single underscores e.g. `htmltools::div(data__foo="bar")` which is
converted to `htmltools::div(data_foo="bar")`.

## Built-in Helpers

Template bodies are evaluated in a freshwater environment that provides
the following helpers:

- [`form()`](https://elianhugh.github.io/freshwater/reference/form.md) —
  form helper with optional CSRF injection and method spoofing.

- [`csrf_token()`](https://elianhugh.github.io/freshwater/reference/csrf_token.md)
  — returns the current CSRF token string

## Template Context

A template render context is maintained during evaluation which is used
for fragment extraction and cache scoping. The template context is
separate from the request context defined elsewhere.

## See also

[document](https://elianhugh.github.io/freshwater/reference/document.md),
[cache](https://elianhugh.github.io/freshwater/reference/template-caching.md),
[form](https://elianhugh.github.io/freshwater/reference/form.md),
[target](https://elianhugh.github.io/freshwater/reference/target.md),
[csrf_token](https://elianhugh.github.io/freshwater/reference/csrf_token.md),
[api_freshwater](https://elianhugh.github.io/freshwater/reference/api_freshwater.md)

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

# Attribute Norming
my_form <- template({
    form(
        label(for_ = "desc", "Description"),
        input(
            type = "text",
            id = "desc",
            data_user__id = "123"
        )
    )
})
my_form()
#> <form method="get">
#>   <label for="desc">Description</label>
#>   <input type="text" id="desc" data-user_id="123"/>
#> </form>
```
