ensure_cache_state <- function() {
  if (is.null(freshwater$cache)) {
    freshwater$cache <- new.env(parent = emptyenv())
  }
  if (is.null(freshwater$cache$backend)) {
    freshwater$cache$backend <- cachem::cache_mem(max_size = 1024 * 1024^2)
  }
  if (is.null(freshwater$cache$store)) {
    set_cache_backend(freshwater$cache$backend)
  }
}

#' @keywords internal
has_store <- function(...) {
  ensure_cache_state()
  freshwater$cache$has_store(...)
}

#' @keywords internal
drop_store <- function(...) {
  ensure_cache_state()
  freshwater$cache$drop_store(...)
}

cache_key <- function(name, vary, id, fragment) {
  rlang::hash(
    list(
      name,
      vary,
      id,
      fragment
    )
  )
}

#' Configure freshwater's cache backend
#'
#' `set_cache_backend()` replaces the cache backend used by [cache()].
#'
#' This function allows controlling cache persistence (memory vs disk),
#' eviction policies, and storage limits via the backend object.
#'
#' @param backend cache backend accepted by [memoise::memoise]
#'
#' @seealso [cache()], [clear_cache()], [memoise::memoise]
#' @export
set_cache_backend <- function(backend) {
  freshwater$cache$backend <- backend
  freshwater$cache$store <- memoise::memoise(
    function(key, fn) fn(),
    hash = function(args) args$key,
    cache = freshwater$cache$backend
  )
  freshwater$cache$has_store <- memoise::has_cache(freshwater$cache$store)
  freshwater$cache$drop_store <- memoise::drop_cache(freshwater$cache$store)

  invisible(NULL)
}

#' Get freshwater's current cache backend
#'
#' Returns the cache backend currently used by [cache()].
#'
#' @return A cache backend object (typically from `cachem`), or `NULL` if the
#'   cache has not yet been initialised.
#'
#' @examples
#' get_cache_backend()
#'
#' @seealso [set_cache_backend()], [cache()]
#' @export
get_cache_backend <- function() {
  freshwater$cache$backend
}

#' Cache a rendered partial within a template
#'
#' `cache()` memoises a portion of HTML output within a freshwater template.
#' The contents are computed once per unique cache key, and reused in subsequent calls.
#' This avoids repeat evaluation of expensive or stable HTML trees.
#'
#' Caches may be freely nested, as each cache is scoped to the template
#' context it is executed in.
#'
#' Caching occurs a small overhead for first-time usage, but is faster in proceeding calls.
#'
#' @details
#'
#' Caching is powered by [memoise]. Cache storage limits,
#' eviction, and persistence are controlled via the underlying
#' memoise/cache backend.
#'
#' @export
#' @param name unique name for the cached partial template
#' @param vary values that should change when the cached output should change. This is used to construct the cache key.
#' @param ... tag content to render and cache
#' @examples
#' # Caching
#' nav <- template(user, {
#'   div(
#'     cache(
#'       "nav",
#'       vary = user$id,
#'       ul(
#'         li("Home"),
#'         li("Profile"),
#'         if (user$is_admin) li("Admin")
#'       )
#'     )
#'   )
#' })
#' nav(list(id = 1, is_admin = TRUE))
#'
#' # Nested Caches
#' dashboard <- template(page = list(), stats = list(), recent = list(), {
#'     cache(
#'         name = "page",
#'         vary = page$updated_at,
#'         div(
#'             h1("Dashboard"),
#'             cache(
#'                 name = "stats",
#'                  vary = stats$updated_at,
#'                 div(p(stats$count))
#'             ),
#'             cache(
#'                 name = "recent",
#'                 vary = recent$updated_at,
#'                 div(recent)
#'             )
#'         )
#'     )
#' })
#' dashboard()
#'
#' # TTL-caching (time-based invalidation)
#' page <- template({
#'   cache(
#'     name = "clock",
#'     vary = memoise::timeout(60L),
#'     div(sprintf("Generated at %s", Sys.time()))
#'   )
#' })
#' page()
#'
#' @seealso [template], [api_cget], [memoise::memoise]
#' @rdname template-caching
cache <- function(name, vary = NULL, ...) {
  ensure_cache_state()

  context <- current_template(parent.frame())
  vary <- force(vary)

  env <- parent.frame()

  expr <- substitute(
    htmltools::tagList(...)
  )

  fn <- function() {
    res <- eval(expr, env) |>
      htmltools::as.tags()

      if (is.null(context$fragment)) {
        htmltools::doRenderTags(res)
      } else {
        res
      }
  }

  key <- cache_key(
    name,
    vary,
    context$id,
    context$fragment
  )

  hit <- has_store(key)
  res <- freshwater$cache$store(key, fn)

  if (hit) {
    class(res) <- c("freshwater_cached_partial", class(res))
  }

  res
}

#' Clear the cache of all memoised templates
#'
#' `clear_cache()` removes all memoised templates from freshwater's cache store.
#'
#' @export
#' @rdname template-caching
clear_cache <- function() {
  memoise::forget(freshwater$cache$store)
  invisible(TRUE)
}


#' Invalidate a specific cached fragment or partial template
#'
#' `invalidate_cache()` removes a single cached entry identified by `name`, and optionally via
#' `vary` and fragment values. Note that the `invalidate_cache` arguments
#' must match those in the original `cache` call, as they are used to construct
#' the cache key.
#'
#' @rdname template-caching
#' @param tpl a template function created by [template()].
#' @param fragment optional fragment name for targetting cached fragments
#' @export
invalidate_cache <- function(tpl, name, vary = NULL, fragment = NULL) {
  id <- attr(tpl, "template_id")
  if (is.null(id)) {
    rlang::abort("Not a freshwater template. Missing id attribute.")
  }

  key <- cache_key(
    name,
    vary,
    id,
    fragment
  )

  if (has_store(key)) {
    drop_store(key = key, fn = \() NULL)
  } else {
    FALSE
  }
}

#' Invalidate a cached partial from within a template
#'
#' `invalidate_cache_here()` is the in-template version of `invalidate_cache`. It uses
#' the current template execution context to allow users to forcibly regenerate caches
#' inside the template function.
#'
#' Note: invalidation affects future renders only. Calling this within the `cache()` block
#' that is being targeted will not result in an invalidation of the cache.
#'
#' @rdname template-caching
#' @examples
#' # Invalidate the current cache
#' # during rendering
#'
#' page <- template(user, {
#'   div(
#'     cache(
#'       name = "content",
#'       vary = user$id,
#'       {
#'         if (user$refresh) {
#'           invalidate_cache_here(
#'             name = "content",
#'             vary = user$id
#'           )
#'         }
#'         p("Hello ", user$id)
#'       }
#'     )
#'   )
#' })
#'
#' page(list(id = 1, refresh = FALSE))
#'
#' page(list(id = 1, refresh = TRUE))
#'
#' @export
invalidate_cache_here <- function(name, vary = NULL, fragment = NULL) {
  ctx <- current_template(parent.frame())
  key <- cache_key(
    name,
    vary,
    ctx$id,
    fragment %||% ctx$fragment
  )
  if (has_store(key)) {
    drop_store(key = key, fn = \() NULL)
  } else {
    FALSE
  }
}

#' @exportS3Method
print.freshwater_cached_partial <- function(x, ...) {
  if (interactive()) {
    cat("[cached partial]\n")
  }
  NextMethod()
}