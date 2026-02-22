validate_hook <- function(hook) {
  if (!is.function(hook)) {
    rlang::abort("Hook must be a function.", class = "freshwater_hook_invalid")
  }

  fmls <- formals(hook)
  nms <- names(fmls)

  if (length(fmls) != 3L || !identical(nms, c("api", "args", "next_call"))) {
    rlang::abort(
      c(
        "Hooks must have exactly three arguments named `api`, `args` and `next_call`.",
        i = paste0(
          "Got: function(",
          paste(ifelse(nzchar(nms), nms, "<unnamed>"), collapse = ", "),
          ")"
        )
      ),
      class = "freshwater_hook_invalid"
    )
  }

  invisible(TRUE)
}

is_system_hook <- function(h) {
  startsWith(hook_id(h), "freshwater::")
}


order_hooks <- function(hooks) {
  system_order <- c(
    "freshwater::context",
    "freshwater::error_pages",
    "freshwater::csrf",
    "freshwater::csrf_context"
  )

  ids <- vapply(hooks, hook_id, character(1), USE.NAMES = FALSE)
  pos <- match(ids, system_order)
  is_sys <- startsWith(ids, "freshwater::")

  rank <- ifelse(
    !is.na(pos),
    pos,
    ifelse(is_sys, Inf, Inf + 1)
  )
  hooks[order(rank)]
}

hook_id <- function(h) {
  attr(h, "freshwater_hook_id", exact = TRUE) %||%
    paste0("user::", rlang::hash(h))
}

add_hook <- function(handler, hook, .where = c("append", "prepend")) {
  .where <- match.arg(.where)

  incoming <- if (is.list(hook)) hook else list(hook)

  for (h in incoming) {
    validate_hook(h)
  }

  current <- attr(handler, "freshwater_hooks", exact = TRUE)
  if (is.null(current)) {
    current <- list()
  }

  hooks <- switch(
    .where,
    append = c(current, incoming),
    prepend = c(incoming, current)
  )

  hooks <- dedupe_hooks(hooks)
  hooks <- order_hooks(hooks)
  attr(handler, "freshwater_hooks") <- hooks

  handler
}

dedupe_hooks <- function(hooks) {
  ids <- vapply(
    hooks,
    function(hook) {
      attr(hook, "freshwater_hook_id", exact = TRUE) %||%
        paste0("user::", rlang::hash(hook))
    },
    character(1L),
    USE.NAMES = FALSE
  )

  hooks[!duplicated(ids)]

}

invoke_hooks <- function(api, handler) {
    force(handler)

    fn <- function(...) {
        args <- list(...)
        i <- 0L
        next_call <- \() {
            hooks <- attr(handler, "freshwater_hooks", exact = TRUE)
            if (is.null(hooks)) hooks <- list()

            i <<- i + 1L
            if (i <= length(hooks)) {
                return(hooks[[i]](api, args, next_call))
            }

            do.call(handler, args)
        }
        next_call()
    }

    attr(fn, "freshwater_hook_wrapper") <- TRUE
    attr(fn, "freshwater_hook_base") <- handler
    fn
}

patch_plumber_handler <- function(api, plumber_handler, hooks, .where = c("append","prepend")) {
    .where <- match.arg(.where)

    plumber_env <- environment(plumber_handler)
    user_function <- plumber_env[["handler"]]

    if (!is.function(user_function)) {
        return(plumber_handler)
    }

    if (isTRUE(attr(user_function, "freshwater_hook_wrapper", exact = TRUE))) {
        base <- attr(user_function, "freshwater_hook_base", exact = TRUE)
        if (is.function(base)) {
            user_function <- base
        }
    }

    user_function <- add_hook(user_function, hooks, .where = .where)
    plumber_env[["handler"]] <- invoke_hooks(api, user_function)
    plumber_handler
}

#' Route handler hooks
#'
#' Add middleware-style hooks to *all
#' existing routes* in a [plumber2] API.
#'
#' Hooks are called iteratively in order of
#' installation (barring if they are prepended), culminating in the
#' final user handler.
#'
#' @details
#'
#' - Routing is managed by [plumber2] -- this function does not change
#' routing precedence. Within the handler itself, however, hooks run in the order
#' they are installed.
#' - The function is idempotent (with respect to either a computed hash of the hook or a
#' provided id), and only new hooks will be installed.
#'
#' @param api a [plumber2] api object.
#' @param hooks a single hook or list of hooks that take the signature
#' `fn(api, args, next_call)`, where `args`
#' is the list of handler arguments.
#' The return value of the hook should be
#' `next_call()` to facilitate calling of subsequent hooks &
#' the user handler function. Not calling next_call() will
#' short-circuit the handler chain.
#' @param .where whether the hooks should be appended or prepended to the
#' list of installed hooks
#'
#' @rdname hooks
#' @export
enhook_routes <- function(api, hooks, .where = c("append", "prepend")) {
  .where <- match.arg(.where)

  rr <- api$request_router
  routes <- rr$routes
  for (route in routes) {
    r <- rr$get_route(route)
    r$remap_handlers(function(method, path, handler) {
      handler <- patch_plumber_handler(api, handler, hooks, .where = .where)
      r$add_handler(method, path, handler)
    })
  }
  api
}


#' @rdname hooks
#' @param id id of the hook
#' @param fn function with signature `fn(api, args, next_call)`
#' @export
hook <- function(id = NULL, fn) {
  is.function(fn) || rlang::abort("Hook `fn` must be a function.")

  validate_hook(fn)

  if (!is.null(id)) {
    if (!is.character(id) || length(id) != 1L || !nzchar(id)) {
      rlang::abort("Hook `id` must be a non-empty character scalar.")
    }
    attr(fn, "freshwater_hook_id") <- id
  }

  fn
}