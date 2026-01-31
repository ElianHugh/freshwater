# Register HTML Serialiser

Registers an HTML serialiser for
[plumber2::plumber2](https://plumber2.posit.co/reference/plumber2-package.html)
that renders shiny tags and taglists via
[`htmltools::doRenderTags()`](https://rstudio.github.io/htmltools/reference/renderTags.html).
This is preferrable over
[`htmltools::renderTags()`](https://rstudio.github.io/htmltools/reference/renderTags.html)
as often we want to be able to emit head tags which
[htmltools::htmltools](https://rstudio.github.io/htmltools/reference/htmltools-package.html)
attempts to hide for shiny usecases.

## Usage

``` r
register_html_serialiser()
```

## Details

The freshwater serialiser safely falls back to the default plumber2
implementation for other classes of inputs.
