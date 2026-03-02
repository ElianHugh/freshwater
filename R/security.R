#' Apply CSRF Protection to a plumber2 API
#'
#' `api_csrf() `installs CSRF middleware on a plumber2 API using
#' the double-submit cookie pattern.
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
#' @seealso [form], [api_freshwater]
#' @export
api_csrf <- function(api, secure = TRUE) {
    if (!requireNamespace("openssl", quietly = TRUE)) {
        rlang::abort("openssl is required to enable CSRF protection.")
    }

    fw_env <- get_freshwater_env(api)

    if (isTRUE(fw_env$csrf$installed)) {
        return(api)
    }

    fw_env$csrf$installed <- TRUE

    api <- api_context(api)
    unsafe_methods <- c("post", "put", "delete", "patch")
    safe_methods <- c("get", "head", "options")
    cookie_name <- if (isTRUE(secure)) "__Host-csrf" else "csrf"

    fw_env$csrf$cookie_name <- cookie_name

    ignored_paths <- c(
        "/__docs__/"
    )

    plumber2::api_on(api, "start", function(...) {
        api$trigger("freshwater_csrf")
    })

    # separate event for testing purposes
    plumber2::api_on(api, "freshwater_csrf", function(...) {
        if (isTRUE(fw_env$csrf$hooked)) {
            return(invisible(NULL))
        }
        fw_env$csrf$hooked <- TRUE
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

                        cookie_name <- fw_env$csrf$cookie_name %||% "csrf"
                        token <- request$cookies[[cookie_name]] %||% ""

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

.csrf_token_impl <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx) || is.null(ctx$csrf_token)) {
        return("")
    }
    ctx$csrf_token()
}

#' CSRF Token
#' @description
#' `csrf_token()` returns the current CSRF token string for
#' the active request when used within a [template()].
#' Intended for custom forms / custom token placement
#' (meta tags, JS fetch, etc).
#'
#' Do not call the `freshwater::csrf_token()`
#' function directly, it is a stub.
#' @examples
#' page <- template({
#'     html(
#'         head(
#'             meta(name = "csrf-token", content = csrf_token())
#'         ),
#'         body(
#'             div("App content")
#'         )
#'     )
#' })
#' page()
#' @seealso [api_csrf]
#' @export
csrf_token <- function() {
    rlang::abort(
        c(
            "freshwater::csrf_token is a stub and cannot be called directly",
            i = "Use csrf_token() inside `template()` rendering when `api_csrf` is installed"
        ),
        class = "freshwater_builtin_stub"
    )
}
