# Package index

## Rendering

- [`template()`](https://elianhugh.github.io/freshwater/reference/templating.md)
  [`fragment()`](https://elianhugh.github.io/freshwater/reference/templating.md)
  : Create a reusable HTML template
- [`register_html_serialiser()`](https://elianhugh.github.io/freshwater/reference/register_html_serialiser.md)
  : Register HTML Serialiser

## Rendering Performance

- [`cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
  [`clear_cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
  [`invalidate_cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
  [`invalidate_cache_here()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
  : Cache a rendered partial within a template

## Context

- [`current_path()`](https://elianhugh.github.io/freshwater/reference/current_path.md)
  : Get current request path from context

## Middleware

- [`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md)
  : Freshwater defaults for plumber2 APIs
- [`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md)
  : Freshwater Error Pages
- [`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md)
  : Apply CSRF Protection to a plumber2 API
- [`api_cget()`](https://elianhugh.github.io/freshwater/reference/api_cget.md)
  : Conditional GET
- [`enhook_routes()`](https://elianhugh.github.io/freshwater/reference/hooks.md)
  [`hook()`](https://elianhugh.github.io/freshwater/reference/hooks.md)
  : Route handler hooks

## Response Helpers

- [`redirect()`](https://elianhugh.github.io/freshwater/reference/redirect.md)
  : Redirect to another resource

## Defaults

- [`default_error_500_template()`](https://elianhugh.github.io/freshwater/reference/freshwater_error_templates.md)
  [`default_error_404_template()`](https://elianhugh.github.io/freshwater/reference/freshwater_error_templates.md)
  [`default_error_403_template()`](https://elianhugh.github.io/freshwater/reference/freshwater_error_templates.md)
  : Error Page Templates
