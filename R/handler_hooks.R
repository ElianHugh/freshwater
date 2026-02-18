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

  attr(handler, "freshwater_hooks") <- unique(switch(
    .where,
    append = c(current, incoming),
    prepend = c(incoming, current)
  ))

  handler
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

#' Handler Hooks
#'
#' Sometimes you need to get the results of a [plumber2] handler *before*
#' it is processed by the server. Or you need to guarantee that some
#' checks occur before any further routing.
#'
#' Order of when handlers are executed isn't really guaranteed.
#'
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
