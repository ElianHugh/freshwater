# Package index

## Rendering

- [`template()`](https://elianhugh.github.io/freshwater/reference/templating.md)
  [`fragment()`](https://elianhugh.github.io/freshwater/reference/templating.md)
  : Create a reusable HTML template
- [`document()`](https://elianhugh.github.io/freshwater/reference/document.md)
  : HTML Document Root
- [`form()`](https://elianhugh.github.io/freshwater/reference/form.md) :
  Form
- [`target()`](https://elianhugh.github.io/freshwater/reference/target.md)
  : Resolve a template's target selector
- [`targets()`](https://elianhugh.github.io/freshwater/reference/targets.md)
  : Combine multiple target selectors
- [`csrf_token()`](https://elianhugh.github.io/freshwater/reference/csrf_token.md)
  : CSRF Token
- [`register_html_serialiser()`](https://elianhugh.github.io/freshwater/reference/register_html_serialiser.md)
  : Register HTML Serialiser
- [`register_async_evaluator()`](https://elianhugh.github.io/freshwater/reference/register_async_evaluator.md)
  : Register context-safe async evaluator

## Rendering Performance

- [`cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
  [`clear_cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
  [`invalidate_cache()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
  [`invalidate_cache_here()`](https://elianhugh.github.io/freshwater/reference/template-caching.md)
  : Cache a partial within a template
- [`set_cache_backend()`](https://elianhugh.github.io/freshwater/reference/set_cache_backend.md)
  : Configure freshwater's cache backend
- [`get_cache_backend()`](https://elianhugh.github.io/freshwater/reference/get_cache_backend.md)
  : Get freshwater's current cache backend

## Context

- [`current_path()`](https://elianhugh.github.io/freshwater/reference/current_context.md)
  [`current_method()`](https://elianhugh.github.io/freshwater/reference/current_context.md)
  [`current_query()`](https://elianhugh.github.io/freshwater/reference/current_context.md)
  [`current_cookie()`](https://elianhugh.github.io/freshwater/reference/current_context.md)
  [`current_header()`](https://elianhugh.github.io/freshwater/reference/current_context.md)
  : Get request data from current context
- [`freshwater_context`](https://elianhugh.github.io/freshwater/reference/freshwater_context.md)
  : Freshwater Request Context

## Routing & URLS

- [`endpoints()`](https://elianhugh.github.io/freshwater/reference/endpoints.md)
  : Reverse Routing
- [`redirect()`](https://elianhugh.github.io/freshwater/reference/redirect.md)
  : Redirect to another resource

## Middleware

- [`api_freshwater()`](https://elianhugh.github.io/freshwater/reference/api_freshwater.md)
  : Freshwater defaults for plumber2 APIs
- [`api_error_pages()`](https://elianhugh.github.io/freshwater/reference/error_pages.md)
  : Freshwater Error Pages
- [`api_csrf()`](https://elianhugh.github.io/freshwater/reference/api_csrf.md)
  : Apply CSRF Protection to a plumber2 API
- [`api_cget()`](https://elianhugh.github.io/freshwater/reference/api_cget.md)
  : Conditional GET
- [`api_hooks()`](https://elianhugh.github.io/freshwater/reference/hooks.md)
  [`hook()`](https://elianhugh.github.io/freshwater/reference/hooks.md)
  : Route handler hooks

## Defaults

- [`default_error_500_template()`](https://elianhugh.github.io/freshwater/reference/freshwater_error_templates.md)
  [`default_error_404_template()`](https://elianhugh.github.io/freshwater/reference/freshwater_error_templates.md)
  [`default_error_403_template()`](https://elianhugh.github.io/freshwater/reference/freshwater_error_templates.md)
  : Error Page Templates
