#' @include handler_hooks.R

#' @title Freshwater Request Context
#' @description
#' freshwater installs a per-request execution context that allows
#' [current_path()], [csrf_token()], and [template()]
#' helpers to access the active HTTP request.
#' The context itself is stored in freshwater's internal state and is
#' set/unset with each request. Context-dependent helpers
#' are only valid when handling an active request. Moreover,
#' requests are only active during *synchronous execution*.
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
#' [api_csrf()], [api_error_pages()], and [register_html_serialiser]
#' based on matching formal parameters.
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
    args_from_fmls <- function(fn, dots) {
        fml_names <- formals(fn) |>
            names()
        dots[which(names(dots) %in% fml_names)]
    }

    dots <- list(...)

    all_fml_names <- names(c(
        formals(api_csrf),
        formals(api_error_pages),
        formals(register_html_serialiser)
    ))

    if (any(!names(dots) %in% all_fml_names)) {
        idx <- which(!names(dots) %in% all_fml_names)
        fmls <- sprintf("`%s`", names(dots)[idx]) |>
            paste0(collapse = ", ")

        fmls <- sprintf(
            "%s %s",
            fmls,
            if (length(idx) == 1) "is" else "are"
        )

        rlang::abort(
            c(
                "Unexpected argument passed.",
                sprintf(
                    "%s invalid to pass to `api_csrf`, `api_error_pages`, and `register_html_serialiser`.",
                    fmls
                )
            )
        )
    }

    serialiser_args <- args_from_fmls(register_html_serialiser, dots)
    do.call(register_html_serialiser, args = serialiser_args)

    api <- api_context(api)

    csrf_args <- args_from_fmls(api_csrf, dots)
    if (csrf) {
        api <- do.call(
            api_csrf,
            args = c(list(api = api), csrf_args)
        )
    } else if (length(csrf_args) > 0L) {
        rlang::warn(
            sprintf(
                "CSRF is disabled. Arguments %s will be ignored.",
                paste0(
                    "`",
                    names(csrf_args),
                    "`",
                    collapse = ", "
                )
            )
        )
    }

    error_page_args <- args_from_fmls(api_error_pages, dots)
    if (error_pages) {
        api <- do.call(
            api_error_pages,
            args = c(list(api = api), error_page_args)
        )
    } else if (length(error_page_args) > 0L) {
        rlang::warn(
            sprintf(
                "Error pages are disabled. Arguments %s will be ignored.",
                paste0(
                    "`",
                    names(error_page_args),
                    "`",
                    collapse = ", "
                )
            )
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

    plumber2::api_on(
        api,
        "freshwater::hook",
        function(server, id, request, arg_list) {
            ensure_endpoints_registered(server, force = TRUE)
        }
    )

    plumber2::api_on(
        api,
        "before-request",
        function(server, id, request, arg_list) {
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
        }
    )

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
                ctx$api <- api
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

#' @title Get request data from current context
#'
#' @description
#' These read-only helpers provide access to request data for the current HTTP
#' request via the freshwater request context.
#'
#' - `current_path()` returns the request URL path
#' - `current_method()` returns the HTTP method
#' - `current_query()` returns the query parameters
#' - `current_cookie()` returns the value of a cookie by name
#' - `current_header()` returns the value
#' of a header by name
#'
#' These functions are primarily intended for use inside templates
#' where a request context has been established. If called
#' outside of an active context, an error is raised.
#'
#' Context is available when freshwater context middleware is
#' active (installed automatically by [api_csrf()],
#' [api_error_pages()], or [api_freshwater()]).
#'
#' @family context helpers
#' @rdname current_context
#' @param name the name of a cookie or header
#' @seealso [api_freshwater()], [api_csrf()], [api_error_pages()]
#' @export
current_path <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) {
        rlang::abort(
            c(
                "freshwater context missing",
                i = "Did you forget to install freshwater middleware via `api_freshwater()`?",
                i = "Helpers like `current_path()` can only be used during a request."
            ),
            class = "freshwater_context_missing"
        )
    }
    ctx$request$path
}


#' @rdname current_context
#' @export
current_method <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) {
        rlang::abort(
            c(
                "freshwater context missing",
                i = "Did you forget to install freshwater middleware via `api_freshwater()`?",
                i = "Helpers like `current_method()` can only be used during a request."
            ),
            class = "freshwater_context_missing"
        )
    }
    ctx$request$method
}

#' @rdname current_context
#' @export
current_query <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) {
        rlang::abort(
            c(
                "freshwater context missing",
                i = "Did you forget to install freshwater middleware via `api_freshwater()`?",
                i = "Helpers like `current_query()` can only be used during a request."
            ),
            class = "freshwater_context_missing"
        )
    }
    ctx$request$query
}

#' @rdname current_context
#' @export
current_cookie <- function(name) {
    ctx <- get_fw_context()
    if (is.null(ctx)) {
        rlang::abort(
            c(
                "freshwater context missing",
                i = "Did you forget to install freshwater middleware via `api_freshwater()`?",
                i = "Helpers like `current_cookie()` can only be used during a request."
            ),
            class = "freshwater_context_missing"
        )
    }
    ctx$request$cookies[[name]]
}

#' @rdname current_context
#' @export
current_header <- function(name) {
    ctx <- get_fw_context()
    if (is.null(ctx)) {
        rlang::abort(
            c(
                "freshwater context missing",
                i = "Did you forget to install freshwater middleware via `api_freshwater()`?",
                i = "Helpers like `current_header()` can only be used during a request."
            ),
            class = "freshwater_context_missing"
        )
    }
    ctx$request$get_header(name)
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

        # for future flags
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

#' Register context-safe async evaluator
#'
#' Registers an asynchronous evaluator for routes, allowing freshwater context to
#' be propogated to [mirai::mirai] workers. This means that contextful helpers such as [current_method],
#' [current_path], and [current_query] (among others) will work in async routes.
#'
#' @details
#' Context is not inherently portable across asynchronous contexts, this function
#' creates a read-only context that is passed to a mirai worker.
#'
#' Hooks are *not* applied to the async route, but may be provided to any associated
#' `then` handlers. If error pages are installed on the main process, errors from
#' the worker will be appropriately converted into freshwater error pages.
#'
#' Requires the [promises::promises], [mirai::mirai],
#' and [mori::mori] packages in order to be used.
#'
#' @examples
#' register_async_evaluator()
#' #* @async
#' #* @get /async
#' function() {
#'  current_path()
#' }
#' @seealso [api_freshwater], [api_error_pages], [api_hooks], [mirai::mirai]
#'
#' @param force whether to register the evaluator regardless of if it has been
#' registered already
#' @param set_default whether to set the default async evaluator to the
#' freshwater version.
#' @export
register_async_evaluator <- function(force = FALSE, set_default = TRUE) {
    if (!requireNamespace("mori", quietly = TRUE)) {
        rlang::abort(
            "{mori} is required to register a freshwater async evaluator."
        )
    }
    if (!requireNamespace("promises", quietly = TRUE)) {
        rlang::abort(
            "{promises} is required to register a freshwater async evaluator."
        )
    }
    if (!requireNamespace("mirai", quietly = TRUE)) {
        rlang::abort(
            "{mirai} is required to register a freshwater async evaluator."
        )
    }

    if (!force && isTRUE(freshwater$async_registered)) {
        return(invisible(NULL))
    }

    current <- getOption("plumber2.async")
    if (isTRUE(set_default)) {
        if (is.null(current) || force) {
            options(plumber2.async = "freshwater")
        }
    }

    plumber2::register_async("freshwater", function(...) {
        function(expr, envir) {
            parent <- sys.parent()
            ctx <- new.env(parent = emptyenv())
            ctx$request <- get("request", envir = parent)
            ctx$api <- get("server", envir = parent)

            with_fw_context(ctx, {
                portable_ctx <- create_portable_context() |>
                    mori::share()

                nm <- mori::shared_name(portable_ctx)

                body <- substitute(
                    {
                        tryCatch(
                            {
                                .fw_ctx <- mori::map_shared(nm)
                                .set_fw_context(.fw_ctx)
                                expr
                            },
                            error = function(e) e
                        )
                    },
                    list(
                        expr = expr,
                        nm = nm,
                        .set_fw_context = set_fw_context
                    )
                )

                promises::hybrid_then(
                    expr = {
                        mirai::mirai(
                            .expr = body,
                            envir,
                            ...
                        )
                    },
                    on_success = function(v) {
                        if (!inherits(v, "error")) {
                            return(v)
                        }

                        api <- ctx$api
                        request <- ctx$request
                        response <- request$response

                        if (!inherits(v, "reqres_problem")) {
                            response$status <- 500L
                        } else {
                            response$status <- v$status
                        }

                        fw_env <- get_freshwater_env(api)
                        if (isTRUE(fw_env$error_pages$installed)) {
                            api$trigger(
                                "error_code",
                                status = response$status,
                                request = request,
                                response = response,
                                message = v
                            )
                            return(
                                list(
                                    result = response$body,
                                    continue = plumber2::Break
                                )
                            )
                        }

                        list(
                            result = "The server hit an error while processing your request.",
                            continue = plumber2::Break
                        )
                    },
                    tee = FALSE
                )
            })
        }
    })

    freshwater$async_registered <- TRUE

    invisible(NULL)
}


#' This *has* to retain parity with the internal fw context definition
#' @noRd
create_portable_context <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) {
        rlang::abort("Unexpected missing freshwater context.")
    }
    fw_env <- get_freshwater_env(ctx$api)

    structure(
        list(
            api = structure(
                list(),
                freshwater = list(
                    endpoints = fw_env$endpoints
                ),
                class = c("freshwater_api", "list")
            ),
            request = list(
                cookies = ctx$request$cookies,
                query = ctx$request$query,
                method = ctx$request$method,
                path = ctx$request$path,
                get_header = "????"
            )
        ),
        class = c("fw_portable_context", "list")
    )
}