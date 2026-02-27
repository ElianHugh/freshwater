#' @include handler_hooks.R

#' Freshwater defaults for plumber2 APIs
#'
#' This is a convenience wrapper around [register_html_serialiser()],
#' [api_csrf()], and [api_error_pages()].
#'
#' @export
#'
api_freshwater <- function(api, csrf = TRUE, error_pages = TRUE, ...) {
    register_html_serialiser()

    api <- api_context(api)

    if (csrf) api <- api_csrf(api, ...)
    if (error_pages) api <- api_error_pages(api, ...)

    invisible(api)
}

#' @keywords internal
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
                    response <- request$response

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

#' @export
current_path <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) return("")
    ctx$request$path
}
