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

#todo, path construction beyond just the glue
make_endpoint <- function(path) {
    params_ <- waysign::path_params(path)
    fn <- function(...) {
        glue::glue_data(list(...), params_$glue) |>
            as.character()
    }
    attr(fn, "meta") <- list(path = path)
    attr(fn, "methods") <- list()
    class(fn) <- c("freshwater_endpoint", class(fn))
    fn
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

register_endpoint <- function(fw_env, route, root, method, path) {
    alias <- endpoint_alias(path)
    endpoint <- make_endpoint(path)
    current <- fw_env$endpoints[[route]][[alias]]

    if (method == "get") {
        if (is.null(current)) {
            fw_env$endpoints[[route]][[alias]] <- structure(
                endpoint,
                has_get = TRUE
            )
        } else {
            methods <- attr(current, "methods", exact = TRUE) %||% list()
            attr(endpoint, "methods") <- methods
            fw_env$endpoints[[route]][[alias]] <- endpoint
        }
        return(invisible(NULL))
    }

    if (is.null(current)) {
        current <- structure(
            function(...) {
                rlang::abort(
                    sprintf(
                        'Endpoint "%s" does not have a GET handler; use $%s().',
                        alias,
                        method
                    )
                )
            },
            meta = list(path = path),
            methods = list(),
            has_get = FALSE,
            class = c("freshwater_endpoint", "function")
        )
    }

    methods <- attr(current, "methods", exact = TRUE) %||% list()
    methods[[method]] <- endpoint
    attr(current, "methods") <- methods
    fw_env$endpoints[[route]][[alias]] <- current
}

register_endpoints <- function(api) {
    rr <- api$request_router
    routes <- rr$routes
    fw_env <- get_freshwater_env(api)

    fw_env$endpoints <- structure(
        list(),
        class = c("freshwater_endpoints", "list")
    )

    for (route in routes) {
        r <- rr$get_route(route)
        # currently unused, requires further testing
        root <- r$root %||% "/"
        root <- sub("^\\^", "", root)

        if (is.null(fw_env$endpoints[[route]])) {
            fw_env$endpoints[[route]] <- list()
        }

        r$remap_handlers(function(method, path, handler) {
            register_endpoint(fw_env, route, root, method, path)
            r$add_handler(method, path, handler)
        })
    }
}

ensure_endpoints_registered <- function(api, force = FALSE) {
    fw_env <- get_freshwater_env(api)
    if (is.null(fw_env$endpoints) || isTRUE(force)) {
        register_endpoints(api)
    }
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
#'  - path parameters are removed from the alias and supplied as function args
#'
#' For example:
#'
#' - `GET /` -> `index()`
#' - `POST /` -> `index$post()`
#' - `GET /my/filter` -> `my_filter()`
#' - `GET /users/:id` -> `users(id = 1)`
#' - `DELETE /users/:id` -> `users$delete(id = 1)`
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
    if (isTRUE(attr(x, "has_get", exact = TRUE))) {
        methods <- c(list("get" = ""), methods)
    }

    meta <- attr(x, "meta", exact = TRUE) %||% list()
    cat(
        "<freshwater endpoint>",
        sprintf("- path: %s", meta$path),
        sprintf("- methods: %s", paste0(names(methods), collapse = ", ")),
        sep = "\n"
    )
}

#' @export
print.freshwater_endpoints <- function(x, ...) {
    cat("<freshwater endpoints> \n")
    for (name in names(x)) {
        cat(
            sprintf("- %s (%s)", name, length(x[[name]]))
        )
    }
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
        !is.na(after) ||
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
