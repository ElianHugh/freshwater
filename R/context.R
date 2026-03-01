#' @include handler_hooks.R

#' @title Freshwater Request Context
#' @description
#' freshwater installs a per-request execution context that allows
#' [current_path()], csrf_token(), and [template()]
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
#' This is a convenience wrapper around
#' [register_html_serialiser()], [api_csrf()], and
#' [api_error_pages()].
#'
#' This installs freshwater request context.
#'
#' @param api a [plumber2] api object.
#' @param csrf whether to enable CSRF protection
#' @param error_pages whether to enable error pages
#' @param ... args passed to either [api_csrf()] or
#' [api_error_pages()]
#'
#' @export
api_freshwater <- function(api, csrf = TRUE, error_pages = TRUE, ...) {
    register_html_serialiser()

    api <- api_context(api)

    if (csrf) api <- api_csrf(api, ...)
    if (error_pages) api <- api_error_pages(api, ...)

    invisible(api)
}

#' @keywords internal
#' @noRd
api_context <- function(api) {
    if (isTRUE(attr(api, "context_installed", exact = TRUE))) {
        return(api)
    }

    attr(api, "context_installed") <- TRUE

    plumber2::api_on(api, "start", function(...) {
        api$trigger("freshwater_context")
    })

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

    plumber2::api_on(api, "freshwater_context", function(...) {
        if (isTRUE(attr(api, "context_hooked", exact = TRUE))) {
            return(invisible(NULL))
        }
        attr(api, "context_hooked") <- TRUE
        enhook_routes(
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
    })

    invisible(api)
}

with_fw_context <- function(ctx, expr) {
    old <- set_fw_context(ctx)
    on.exit(set_fw_context(old), add = TRUE)

    res <- force(expr)

    if (inherits(res, "promise")) {
        res <- promises::with_promise_domain(
            promises::new_promise_domain(
                wrapOnFulfilled = function(onFulfilled) {
                    function(value) {
                        old <- set_fw_context(ctx)
                        on.exit(set_fw_context(old), add = TRUE)
                        onFulfilled(value)
                    }
                },
                wrapOnRejected = function(onRejected) {
                    function(reason) {
                        old <- set_fw_context(ctx)
                        on.exit(set_fw_context(old), add = TRUE)
                        onRejected(reason)
                    }
                }
            ),
            res
        )
    }
    res
}

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
