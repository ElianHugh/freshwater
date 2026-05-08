# Resolve a template's target selector

`target()` returns a CSS selector for a template instance. For normal
targets, this is an id selector of the form `[id="<TEMPLATE_ID>"]`,
where the id is resolved from the template's id function.

## Usage

``` r
target(tpl, ..., .part = NULL)
```

## Arguments

- tpl:

  a freshwater template

- ...:

  arguments passed to the template id function

- .part:

  whether to select a sub-part

## Details

In order to support multi-root template selection, supplying `.part`
will return a data attribute selector of the form:
`[data-fw-part="<TEMPLATE_ID>-<PART_NAME>"]` Part names are
automatically scoped against the template's `.id`, ensuring unique data
attributes across templates.

## See also

[targets](https://elianhugh.github.io/freshwater/reference/targets.md),
[template](https://elianhugh.github.io/freshwater/reference/templating.md)

## Examples

``` r
card <- template(
    user,
    .id = function(user) sprintf("user-%s", user$id),
    {
        div(user$name)
    }
)
target(card, list(id = 1234L))
#> [1] "[id=\"user-1234\"]"

user_table <- template(users = list(), .id = "my-table", {
    table(
        thead(
            .part = "header",
            tr(th("Name"))
        ),
        tbody(
            .part = "body",
            lapply(users, \(user) tr(td(user)))
        ),
        tfoot(
            .part = "footer",
            tr(td(sprintf("There are '%s' users.", length(users))))
        )
   )
})
target(user_table, .part = "body")
#> [1] "[data-fw-part=\"my-table-body\"]"
```
