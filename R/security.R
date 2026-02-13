#' Apply CSRF Protection to a plumber2 API
#'
#' Installs CSRF middleware on a plumber2 API using the double-submit cookie pattern.
#'
#' When installed:
#' - Any `form` element inside [freshwater::template] automatically
#' include a CSRF token.
#' - If working in JavaScript contexts, the `csrf_token()` helper is also accessible inside templates.
#'
#' Middleware behaviour:
#' - On **safe** methods (`GET`, `HEAD`, `OPTIONS`), if the CSRF cookie is missing,
#'   a new token is generated and set as a cookie.
#' - On **unsafe** methods (`POST`, `PUT`, `DELETE`, `PATCH`), the request is
#'   rejected with **403** unless a token provided via the `X-CSRF-Token` header
#'   or a `csrf_token` field in the parsed request body matches the CSRF cookie.
#'
#' @param api a plumber2 API object
#' @param secure if `TRUE`, sets the CSRF cookie to "__Host-csrf" and marks the cookie as
#' secure. If false, uses "csrf".
#'
#' @examples
#' #* @plumber
#' function(api) {
#'   api |>
#'        api_csrf(secure = FALSE)
#' }
#'
#' @export
api_csrf <- function(api, secure = TRUE) {
    if (!requireNamespace("openssl", quietly = TRUE)) {
        rlang::abort("openssl is required to enable CSRF protection.")
    }

    if (isTRUE(attr(api, "csrf_installed", exact = TRUE))) {
        return(api)
    }

    attr(api, "csrf_installed") <- TRUE


    unsafe_methods <- c("post", "put", "delete", "patch")
    safe_methods <- c("get", "head", "options")
    cookie_name <- if (isTRUE(secure)) "__Host-csrf" else "csrf"
    freshwater$csrf_cookie_name <- cookie_name

    ignored_paths <- c(
        "/__docs__/"
    )

    csrf_handler <- function(request, response, body) {
        if (startsWith(request$path, ignored_paths)) {
            return(plumber2::Next)
        }

        cookie_token <- request$cookies[[cookie_name]] %||% ""

        method <- request$method

        if (method %in% safe_methods && !nzchar(cookie_token)) {
            cookie_token <- csrf_new_token()
            response$set_cookie(
                name = cookie_name,
                value = cookie_token,
                path = "/",
                http_only = FALSE,
                secure = secure,
                same_site = "Lax"
            )
        }

        plumber2::Next
    }

    csrf_validator <- function(request, response, body) {
        if (startsWith(request$path, ignored_paths)) {
            return(plumber2::Next)
        }

        cookie_token <- request$cookies[[cookie_name]] %||% ""
        method <- request$method

        if (method %in% unsafe_methods) {
            token <- request$get_header("x-csrf-token") %||%
                body$csrf_token

            if (is.null(token) || !identical(token, cookie_token)) {
                response$status <- 403L
                response$set_header("Content-Type", "text/plain")
                response$body <- "Invalid CSRF token"

                return(plumber2::Break)
            }
        }
        plumber2::Next
    }

    plumber2::api_add_route(api, "csrf", header = TRUE, after = 0L)
    plumber2::api_add_route(api, "csrf_req", header = FALSE, after = 0L)
    plumber2::api_any_header(api, path = "/*", handler = csrf_handler, route = "csrf")
    plumber2::api_any(api, path = "/*", handler = csrf_validator, route = "csrf_req")

    plumber2::api_on(api, "start", function(...) {
        api$trigger("freshwater")
    })

    # separate event for testing purposes
    plumber2::api_on(api, "freshwater", function(...) {
         if (isTRUE(attr(api, "csrf_hooked", exact = TRUE))) {
            return(invisible(NULL))
        }
        attr(api, "csrf_hooked") <- TRUE
        enhook_routes(api, csrf_hook)
    })

    api
}

csrf_new_token <- function() {
    openssl::rand_bytes(64) |>
        format() |>
        paste0(collapse="")
}

set_fw_context <- function(ctx) {
    old <- freshwater$request_context %||% NULL
    freshwater$request_context <- ctx
    old
}

get_fw_context <- function() {
    freshwater$request_context %||% NULL
}

csrf_token <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) return("")
    ctx$csrf_token()
}

form <- function(...) {
    ctx <- get_fw_context()
    if (!is.null(ctx)) {
        children <- list(
            htmltools::tags$input(
                type = "hidden",
                name = "csrf_token",
                value = csrf_token()
            ),
            ...
        )
    } else {
        children <- list(...)
    }
    do.call(htmltools::tags$form, args = children)
}

csrf_hook <- function(args, next_call) {
    request <- args$request

    if (is.null(request)) {
        return(do.call(user_fn, args))
    }

    cookie_name <- freshwater$csrf_cookie_name %||% "RSC-XSRF"
    token <- request$cookies[[cookie_name]] %||% ""

    # set current req context
    # todo, see if this is async compat
    ctx <- new.env(parent = emptyenv())
    ctx$csrf_token <- function() token
    ctx$request <- request

    old <- set_fw_context(ctx)
    on.exit(set_fw_context(old), add = TRUE)

    next_call()
}
