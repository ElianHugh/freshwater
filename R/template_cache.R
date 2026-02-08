#' @keywords internal
store <- memoise::memoise(
  function(key, fn) fn(),
  hash = function(args) args$key
)

#' @keywords internal
has_store <- memoise::has_cache(store)

#' @keywords internal
drop_store <- memoise::drop_cache(store)

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
#' @seealso [template]
#' @rdname template-caching
cache <- function(name, vary = NULL, ...) {
  context <- current_template(parent.frame())
  vary <- force(vary)

  env <- parent.frame()

  expr <- substitute(
    htmltools::tagList(...)
  )

  fn <- function() {
    eval(expr, env) |>
      htmltools::as.tags() |>
      htmltools::doRenderTags()
  }

  key <- cache_key(
    name,
    vary,
    context$id,
    context$fragment
  )

  hit <- has_store(key)
  res <- store(key, fn)

  if (hit) {
    class(res) <- c("freshwater_cached_partial", class(res))
  }

  res
}

#' Clear the cache of all memoised templates
#' @export
#' @rdname template-caching
clear_cache <- function() {
  memoise::forget(store)
  invisible(TRUE)
}

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
