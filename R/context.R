#' @include handler_hooks.R

#' @title Freshwater Request Context
#' @description
#' freshwater installs a per-request execution context that allows
#' [current_path()], [csrf_token()], and [template()]
#' helpers to access the active HTTP request.
#' The context itself is stored in freshwater's internal state and is
#' set/unset with each request.
#'
#' Context is created automatically when
#' [api_freshwater()], [api_csrf()], or [api_error_pages()] is installed.
#'
#' Method spoofing is applied during the `before-request` phase by rewriting
#' `REQUEST_METHOD` when a hidden `_method` field is present.
#'
#' @section Lifecycle:
#' The context exists only during an active HTTP request.
#' Calling context-dependent helpers outside a request will raise
#' a `freshwater_context_missing` error.
#'
#' @seealso [api_freshwater()], [current_path()]
#' @name freshwater_context
NULL


#' @title Freshwater defaults for plumber2 APIs
#'
#' @description
#' Installs freshwater defaults onto a plumber2 API.
#'
#' This is a convenience wrapper:
#' - Registers freshwater's HTML serialiser
#' - Installs freshwater request context
#' - Optionally enables CSRF protection
#' - Optionally installs HTML error page handlers
#'
#' Arguments in `...` are selectively forwarded to
#' [api_csrf()] and [api_error_pages()] based on
#' matching formal parameters.
#'
#' @param api a [plumber2] api object.
#' @param csrf whether to enable CSRF protection
#' @param error_pages whether to enable error pages
#' @param ... args passed to either [api_csrf()] or
#' [api_error_pages()]
#'
#' @seealso [api_csrf], [api_error_pages], [register_html_serialiser]
#' @export
api_freshwater <- function(api, csrf = TRUE, error_pages = TRUE, ...) {
    register_html_serialiser()

    dots <- list(...)
    api <- api_context(api)

    if (csrf) {
        csrf_fmls <- formals(api_csrf) |>
            names()
        csrf_args <- dots[which(names(dots) %in% csrf_fmls)]
        api <- do.call(
            api_csrf,
            args = c(list(api = api), csrf_args)
        )
    }

    if (error_pages) {
        error_pages_fmls <- formals(api_error_pages) |>
            names()
        error_page_args <- dots[which(names(dots) %in% error_pages_fmls)]
        api <- do.call(
            api_error_pages,
            args = c(list(api = api), error_page_args)
        )
    }

    invisible(api)
}

#' @keywords internal
#' @noRd
api_context <- function(api) {
    fw_env <- get_freshwater_env(api)
    if (isTRUE(fw_env$context$installed)) {
        return(api)
    }

    fw_env$context$installed <- TRUE

    plumber2::api_on(api, "before-request", function(server, id, request, arg_list) {
        if (is.null(request)) {
            return(TRUE)
        }
        if (request$method == "post") {
            try(request$parse(plumber2::get_parsers()))
            req_body <- request$body
            if (!is.null(req_body) && "_method" %in% names(req_body)) {
                method <- req_body[["_method"]]
                if (tolower(method) %in% c("delete", "patch", "put")) {
                    origin <- request$origin
                    origin$REQUEST_METHOD <- toupper(method)
                    request$origin <- origin
                }
            }
        }
        TRUE
    })

    api_hooks(
        api,
        list(
            hook("freshwater::context", function(api, args, next_call) {
                request <- args$request

                if (is.null(request)) {
                    return(next_call())
                }

                ctx <- new.env(parent = emptyenv())
                ctx$request <- request
                with_fw_context(ctx, next_call())
            })
        ),
        .where = "prepend"
    )

    invisible(api)
}

#' env is stored in the plumber2 api object via
#' an attribute, used for storing freshwater data
#' on a per-api basis (e.g. if fns are hooked or not)
#' @noRd
get_freshwater_env <- function(api) {
    # mark as a freshwater-plumber2 api so
    # we can add some things to print
    cls <- class(api)
    if (!("freshwater_api" %in% cls)) {
        class(api) <- c("freshwater_api", cls)
    }

    e <- attr(api, "freshwater", exact = TRUE)
    if (is.null(e)) {
        e <- new.env()
        attr(api, "freshwater") <- e
    }
    e
}

with_fw_context <- function(ctx, expr) {
    old <- set_fw_context(ctx)
    on.exit(set_fw_context(old), add = TRUE)

    res <- force(expr)

    res
}

# this is per-session based atm
# ideally this is per-api scoped (and store it on the fw_env attr)
# but we can't make context injection "magic" otherwise
# (or haven't figured out how to yet)
set_fw_context <- function(ctx) {
    old <- freshwater$request_context %||% NULL
    freshwater$request_context <- ctx
    old
}

get_fw_context <- function() {
    freshwater$request_context %||% NULL
}

#' @title Get current request path from context
#'
#' @description
#' Return the URL path of the current HTTP request,
#' as captured in the freshwater request context. If
#' called outside of an active context, an error is
#' raised.
#'
#' This is primarily intended for use inside templates
#' where the request context has been established.
#'
#' Context is available when freshwater context middleware is
#' active (installed automatically by [api_csrf()],
#' [api_error_pages()], or [api_freshwater()]).
#'
#' @family context helpers
#' @seealso [api_freshwater()], [api_csrf()], [api_error_pages()]
#' @export
current_path <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) {
        rlang::abort(
            "freshwater context missing",
            i = "Did you forget to install freshwater middleware via `api_freshwater()`?",
            i = "Helpers like `current_path()` can only be used during a request.",
            class = "freshwater_context_missing"
        )
    }
    ctx$request$path
}


#' @exportS3Method
print.freshwater_api <- function(x, ...) {
    NextMethod()

    fw <- attr(x, "freshwater", exact = TRUE)
    if (is.null(fw)) {
        return(invisible(x))
    }
    cat("\n<freshwater>\n")
    if (isTRUE(fw$context$installed)) {
        cat("  - context: installed\n")
    }
    if (isTRUE(fw$error_pages$installed)) {
        extras <- character(0)

        # for futur flags
        if (isTRUE(fw$error_pages$debug)) {
            extras <- c(extras, " (debug)")
        }
        out <- sprintf("  - error pages: installed%s\n", extras)
        cat(out)
    }
    if (isTRUE(fw$csrf$installed)) {
        cat("  - csrf: installed\n")
    }
    invisible(x)
}