endpoint_alias <- function(path) {
    if (path == "/" || path == "") return("index")

    x <- path |>
        gsub("^/+", "", x = _) |>
        gsub("[:<][^/>]+[>]?", "", x = _) |>
        gsub("/+", "_", x = _) |>
        gsub("_+$", "", x = _)

    if (!nzchar(x)) x <- "index"

    x
}


#' @export
`$.freshwater_endpoint` <- function(x, name) {
    methods <- attr(x, "methods", exact = TRUE) %||% list()
    out <- methods[[name]]
    if (is.null(out)) {
        rlang::abort(
            sprintf('Method "%s" is not available for this endpoint.', name)
        )
    }
    out
}

#' @export
`[[.freshwater_endpoint` <- `$.freshwater_endpoint`

register_endpoints <- function(api) {
    rr <- api$request_router
    routes <- rr$routes
    fw_env <- get_freshwater_env(api)

    fw_env$endpoints <- structure(
        list(),
        class = c("freshwater_endpoints", "list")
    )

    endpoints <- list()
    for (route in routes) {
        r <- rr$get_route(route)
        # currently unused, requires further testing
        # does not appear to be needed?
        root <- r$root %||% "/"
        root <- sub("^\\^", "", root)

        if (is.null(fw_env$endpoints[[route]])) {
            fw_env$endpoints[[route]] <- list()
        }

        r$remap_handlers(function(method, path, handler) {
            alias <- endpoint_alias(path)
            params <- waysign::path_params(path)
            candidate <- list(
                method = method,
                path = path,
                keys = params$keys,
                glue = params$glue
            )

            fw_env$endpoints[[route]][[alias]][[
                length(fw_env$endpoints[[route]][[alias]]) + 1L
            ]] <<- candidate

            r$add_handler(method, path, handler)
        })
    }
    fw_env$endpoints <- compile_aliases(fw_env$endpoints)
    NULL
}

compile_aliases <- function(eps) {
    compiled <- setNames(
        lapply(
            eps,
            function(route_eps) {
                setNames(
                    lapply(
                        names(route_eps),
                        function(alias) {
                            candidates <- route_eps[[alias]]

                            method_names <- unique(vapply(
                                candidates,
                                `[[`,
                                character(1),
                                "method"
                            ))

                            methods <- setNames(
                                lapply(
                                    method_names,
                                    function(method) {
                                        method_candidates <- Filter(
                                            \(x) identical(x$method, method),
                                            candidates
                                        )

                                        fn <- function(..., .query = character(), .anchor = character()) {
                                            dots <- list(...)
                                            nms <- names(dots) %||% character()

                                            hits <- vapply(
                                                method_candidates,
                                                \(candidate) {
                                                    identical(
                                                        sort(candidate$keys),
                                                        sort(nms)
                                                    )
                                                },
                                                logical(1L)
                                            )

                                            if (!any(hits)) {
                                                params_txt <- if (length(nms)) {
                                                    paste(
                                                        sprintf('"%s"', nms),
                                                        collapse = ", "
                                                    )
                                                } else {
                                                    "<none>"
                                                }
                                                keys <- lapply(
                                                    method_candidates,
                                                    \(candidate) { candidate$keys }
                                                ) |> paste0(collapse = ", ")

                                                rlang::abort(
                                                    sprintf(
                                                        'No matching overload for endpoint "%s$%s()" with parameter(s) %s. Expected %s.',
                                                        alias,
                                                        method,
                                                        params_txt,
                                                        keys
                                                    )
                                                )
                                            }

                                            candidate <- method_candidates[[which(
                                                hits
                                            )]]


                                            out <- glue::glue_data(
                                                dots,
                                                candidate$glue
                                            )

                                            paste0(
                                                out,
                                                make_query(.query),
                                                make_anchor(.anchor)
                                            )
                                        }

                                        structure(
                                            fn,
                                            method = method,
                                            paths = vapply(method_candidates, `[[`, character(1), "path"),
                                            class = c("freshwater_endpoint_method", "function")
                                        )
                                    }
                                ),
                                method_names
                            )

                            index <- methods$get

                            if (is.null(index)) {
                                 index <- function(...) {
                                    rlang::abort(
                                        sprintf(
                                            'Method "get" is not available for endpoint "%s".',
                                            alias
                                        )
                                    )
                                }
                            }
                            structure(
                                index,
                                methods = methods,
                                paths = vapply(candidates, `[[`, character(1), "path"),
                                class = c("freshwater_endpoint", "function")
                            )
                        }
                    ),
                    names(route_eps)
                )
            }
        ),
        names(eps)
    )

    structure(
        compiled,
        class = "freshwater_endpoints"
    )
}

ensure_endpoints_registered <- function(api, force = FALSE) {
    fw_env <- get_freshwater_env(api)
    if (is.null(fw_env$endpoints) || isTRUE(force)) {
        register_endpoints(api)
    }
}

make_query <- function(.query) {
    if (is.null(.query) || !length(.query)) {
        return("")
    }

    if (is.null(names(.query)) || any(!nzchar(names(.query)))) {
        rlang::abort("`.query` must be a named list.")
    }

    enc <- lapply(
        seq_along(.query),
        function(i) {
            v <- .query[[i]]
            nm <- names(.query)[[i]]

            sprintf(
                "%s=%s",
                URLencode(as.character(nm), reserved = TRUE, repeated = TRUE),
                URLencode(as.character(v), reserved = TRUE, repeated = TRUE)
            )
        }
    )
    enc <- unlist(enc, use.names = FALSE)
    if (!length(enc)) return("")
    sprintf(
        "?%s",
        paste0(enc, collapse="&")
    )
}

make_anchor <- function(.anchor) {
    if (length(.anchor) > 0L && (is.null(.anchor) || !nzchar(.anchor))) return ("")
    .anchor <- sub("^#+", "", .anchor)
    sprintf("#%s", URLencode(.anchor, reserved = TRUE, repeated = TRUE))
}

#' Reverse Routing
#'
#' Access generated endpoint URL helpers.
#'
#' @details
#' Alias rules:
#'  - "/" endpoints become "index"
#'  - GET endpoints are accessed directly, like index()
#'  - non-GET endpoints require an accessor, like index$delete()
#'  - path parameters are removed from the alias and used to disambiguate overloaded helpers via named
#' function args
#' - Reserved argument: `.query` argument constructs a query from a named list
#' - Reserved argument: `.anchor` constructs an anchor from a character scalar
#'
#' For example:
#'
#' - `GET /` -> `index()`
#' - `POST /` -> `index$post()`
#' - `GET /my/filter` -> `my_filter()`
#' - `GET /users/:id` -> `users(id = 1, .query = list(page = 2))`
#' - `GET /users/:id` -> `users(id = 1, .anchor = "details")`
#' - `GET /users/:id` -> `users(id = 1, .query = list(page = 2), .anchor = "details")`
#' - `DELETE /users/:id` -> `users$delete(id = 1)`
#' - `DELETE /users/:name` -> `users$delete(name = "Jim")`
#'
#' @examples
#' #* @plumber
#' function(api) {
#'    api |>
#'      api_freshwater()
#' }
#'
#' #* @get /
#' #* @serializer html
#' #* @routeName user
#' function() {
#'      endpoints("user")$index()
#' }
#' @param route the route group to retrieve endpoints from; typically defined either via the file name, routeName or route in plumber2. If NULL, will return all endpoints for all routes.
#' @param api a [plumber2] api object. If NULL, context is used to find the api.
#' @param refresh force refresh the registered routes. Useful if you have added routes after calling `endpoints()`.
#' @returns If `route` is `NULL`, returns a list of route groups
#' and their endpoints. Otherwise returns a list of a
#' route's endpoint accessors.
#' @export
endpoints <- function(route = NULL, api = NULL, refresh = FALSE) {
    if (is.null(api)) {
        ctx <- get_fw_context()
        if (is.null(ctx)) {
            rlang::abort(
                "`endpoints()` requires an active freshwater context, or supply `api` explicitly."
            )
        }
        api <- ctx$api
    }

    ensure_endpoints_registered(api, force = refresh)

    fw_env <- get_freshwater_env(api)
    endpoints <- fw_env$endpoints %||% list()
    if (is.null(route)) return(endpoints)

    out <- endpoints[[route]]
    if (is.null(out)) {
        rlang::abort(
            sprintf('Endpoints for route: "%s" do not exist', route)
        )
    }
    out
}


#' @export
print.freshwater_endpoint <- function(x, ...) {
    methods <- attr(x, "methods", exact = TRUE) %||% list()
    paths <- attr(x, "paths", exact = TRUE) %||% list()
    cat(
        "<freshwater endpoint>",
        sprintf("- paths: %s", paste0(paths, collapse = ", ")),
        sprintf("- methods: %s", paste0(names(methods), collapse = ", ")),
        sep = "\n"
    )
    invisible(x)
}

#' @export
print.freshwater_endpoints <- function(x, ...) {
    cat("<freshwater endpoints> \n")
    for (name in names(x)) {
        cat(
            sprintf("- %s (%s)", name, length(x[[name]])),
            "\n"
        )
    }
    invisible(x)
}

#' @export
print.freshwater_endpoint_method <- function(x, ...) {
    method <- attr(x, "method", exact = TRUE) %||% list()
    paths <- attr(x, "paths", exact = TRUE) %||% list()

    cat(
        "<freshwater endpoint method>",
        sprintf("- method: %s", method),
        sprintf("- paths: %s", paste(paths, collapse = ", ")),
        sep = "\n"
    )

    invisible(x)
}


#' Redirect to another resource
#'
#' If `after` is NULL, sends a 303 response and halts request processing.
#' Client is redirected to the given location. This is commonly used in
#' Post/Redirect/GET (PRG) setups to redirect clients to a new page following
#' form submissions.
#'
#' If after is a numeric, a "Refresh" header is attached to the response,
#' instructing the browser to navigate to `location` after the specified number
#' of seconds.
#'
#' @param response [reqres::Response] object
#' @param location path or url to redirect to
#' @param after optional number of seconds to wait before redirection
#'
#' @details
#' The delayed redirect uses the non-standard "Refresh" HTTP header which is
#' widely supported by browsers but is not part of the official HTTP specification.
#' It should not be relied on for API & non-browser clients.
#'
#' See also:
#' - <https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Redirections>
#' - <https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Refresh>
#'
#' @return
#' - [plumber2::Break] when issuing an immediate redirect.
#' - [plumber2::Next] when issuing a delayed navigation.
#'
#' @examples
#' # Immediate redirect (PRG pattern)
#' #* @get /
#' function(response) {
#'     print("Hello!")
#'     redirect(response, "/foo")
#' }
#'
#' # Delayed redirect after rendering content
#' #* @get /count/<n>
#' function(n, response) {
#'   redirect(response, "/", after = 1)
#'   paste("n =", n)
#' }
#' @export
redirect <- function(response, location, after = NULL) {
    location <- gsub("[\r\n]", "", location)
    if (is.null(after)) {
        response$status <- 303L
        response$set_header("Location", location)
        return(plumber2::Break)
    } else {
        after <- as.integer(after)
        (!is.na(after) && after > 0L) ||
            rlang::abort(
                c(
                    "`after` should be a number greater than 0L.",
                    sprintf("Got `%s`", after)
                )
            )
        response$set_header(
            "Refresh",
            sprintf("%s; url=%s", after, location)
        )
    }
    plumber2::Next
}
