#' Apply CSRF Protection to a plumber2 API
#'
#' Installs CSRF middleware on a plumber2 API using the double-submit cookie pattern.
#'
#' When installed:
#' - Any `form` element inside [template] automatically
#' includes a CSRF token.
#' - If working in JavaScript contexts, the `csrf_token()` helper is also accessible inside templates.
#'
#' Middleware behaviour:
#' - On **safe** methods (`GET`, `HEAD`, `OPTIONS`), if the CSRF cookie is missing,
#'   a new token is generated and set as a cookie.
#' - On **unsafe** methods (`POST`, `PUT`, `DELETE`, `PATCH`), the request is
#'   rejected with **403 Forbidden** unless a token provided via the `X-CSRF-Token` header
#'   or a `csrf_token` field in the parsed request body matches the CSRF cookie.
#'
#' This middleware installs freshwater request context.
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

    api <- api_context(api)
    unsafe_methods <- c("post", "put", "delete", "patch")
    safe_methods <- c("get", "head", "options")
    cookie_name <- if (isTRUE(secure)) "__Host-csrf" else "csrf"
    freshwater$csrf_cookie_name <- cookie_name

    ignored_paths <- c(
        "/__docs__/"
    )

    plumber2::api_on(api, "start", function(...) {
        api$trigger("freshwater_csrf")
    })

    # separate event for testing purposes
    plumber2::api_on(api, "freshwater_csrf", function(...) {
        if (isTRUE(attr(api, "csrf_hooked", exact = TRUE))) {
            return(invisible(NULL))
        }
        attr(api, "csrf_hooked") <- TRUE
        enhook_routes(
            api,
            list(
                hook(
                    id = "freshwater::csrf",
                    function(api, args, next_call) {
                        request <- args$request
                        response <- args$response
                        body <- args$body

                        if (is.null(request) || startsWith(request$path, ignored_paths)) {
                            return(next_call())
                        }

                        cookie_token <- request$cookies[[cookie_name]] %||% ""

                        if (
                            request$method %in%
                                safe_methods &&
                                !nzchar(cookie_token)
                        ) {
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

                        if (request$method %in% unsafe_methods) {
                            token <- request$get_header("x-csrf-token") %||%
                                body$csrf_token
                            if (
                                is.null(token) ||
                                    !identical(token, cookie_token)
                            ) {
                                response$status <- 403L
                                response$body <- "Invalid CSRF token"

                                api$trigger(
                                    "error_code",
                                    status = 403L,
                                    request = request,
                                    response = response,
                                    message = NULL
                                )

                                return(plumber2::Break)
                            }
                        }

                        next_call()
                    }
                ),
                hook(
                    id = "freshwater::csrf_context",
                    function(api, args, next_call) {
                        request <- args$request

                        if (is.null(request)) {
                            return(next_call())
                        }

                        cookie_name <- freshwater$csrf_cookie_name %||% "csrf"
                        token <- request$cookies[[cookie_name]] %||% ""

                        # set current req context
                        # todo, see if this is async compat
                        ctx <- get_fw_context()
                        ctx$csrf_token <- function() token
                        ctx$request <- request


                        next_call()
                    }
                )
            ),
            .where = "append"
        )
    })

    api
}

csrf_new_token <- function() {
    openssl::rand_bytes(64) |>
        format() |>
        paste0(collapse = "")
}

csrf_token <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx) || is.null(ctx$csrf_token)) {
        return("")
    }
    ctx$csrf_token()
}

form <- function(..., method = "get") {
    ctx <- get_fw_context()
    children <- list(...)
    method <- tolower(method)

    valid <- c("get", "post", "put", "patch", "delete")
    if (!method %in% valid) {
        rlang::abort(paste0("Unsupported form method: ", method))
    }

    if (!is.null(ctx)) {
        if (!is.null(ctx$csrf_token)) {
            token_input <- htmltools::tags$input(
                type = "hidden",
                name = "csrf_token",
                value = csrf_token()
            )
            children <- c(list(token_input), children)
        }
    }
    if (method %in% c("put", "patch", "delete")) {
        faux_method <- htmltools::tags$input(
            type = "hidden",
            name = "_method",
            value = toupper(method)
        )
        children <- c(list(faux_method), children)
        method <- "post"
    }

    do.call(
        htmltools::tags$form,
        c(list(method = method), children)
    )
}
