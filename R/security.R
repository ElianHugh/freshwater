#' Apply CSRF Protection to a plumber2 API
#'
#' @description
#' `api_csrf()` installs CSRF middleware on a plumber2 API using
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
#' @details
#' # Annotation Reference
#'
#' CSRF exemptions can be specified by `@csrf`:
#' - `"on"`: (default) CSRF checks are enforced
#' - `"off"` or `"exempt"`: CSRF checks are skipped for the route
#'
#' ```r
#' #* @post /foo/*/bar
#' #* @csrf exempt
#' function() {
#'  print("No checking!")
#' }
#' ```
#'
#' # Background
#' Cross-site request forgery (CSRF) refers to attacks that
#' trick user browsers into making unintended unsafe HTTP requests
#' to trusted sites -- often through piggybacking on existing
#' authenticated user sessions.
#'
#' In general, clients are most vulnerable when only cookies are used to
#' validate requests from authenticated users.
#' Read more: <https://developer.mozilla.org/en-US/docs/Web/Security/Attacks/CSRF>
#'
#' @param api a plumber2 API object
#' @param secure if `TRUE`, sets the CSRF cookie to "__Host-csrf" and marks the cookie as
#' secure. If false, uses "csrf".
#' @param exemptions character vector of route patterns to exempt from CSRF checks
#'
#' @examples
#' #* @plumber
#' function(api) {
#'   api |>
#'        api_csrf(secure = FALSE, exemptions = c("/foo/*", "/bar"))
#' }
#'
#'
#' @seealso [form], [api_freshwater], [api_hooks]
#' @export
api_csrf <- function(api, secure = TRUE, exemptions = character()) {
    if (!requireNamespace("openssl", quietly = TRUE)) {
        rlang::abort("openssl is required to enable CSRF protection.")
    }

    if (!is.character(exemptions)) {
        rlang::abort(
            "`exemptions` must be a character vector of path patterns."
        )
    }

    fw_env <- get_freshwater_env(api)

    if (isTRUE(fw_env$csrf$installed)) {
        return(api)
    }

    fw_env$csrf$installed <- TRUE

    for (exemption in exemptions) {
        csrf_exempt_add(api, exemption)
    }

    api <- api_context(api)
    unsafe_methods <- c("post", "put", "delete", "patch")
    safe_methods <- c("get", "head", "options")
    cookie_name <- if (isTRUE(secure)) "__Host-csrf" else "csrf"

    fw_env$csrf$cookie_name <- cookie_name
    fw_env$csrf$secure <- isTRUE(secure)
    fw_env$csrf$safe_methods <- safe_methods
    fw_env$csrf$unsafe_methods <- unsafe_methods

    api <- api_hooks(
        api,
        list(
            hook(
                id = "freshwater::csrf",
                function(api, args, next_call) {
                    request <- args$request
                    response <- args$response
                    body <- args$body
                    ensure_csrf_token(
                        api,
                        request,
                        response,
                        body,
                        fw_env,
                        next_call
                    )
                }
            ),
            hook(
                id = "freshwater::csrf_context",
                function(api, args, next_call) {
                    request <- args$request
                    response <- args$response
                    csrf_context(api, request, response, fw_env, next_call)
                }
            )
        ),
        .where = "append"
    )

    api
}


ensure_csrf_token <- function(api, request, response, body, fw_env, next_call, is_worker = FALSE) {
    if (is.null(request)) {
        return(next_call())
    }

    if (!is.null(fw_env$csrf$exempt)) {
        match <- fw_env$csrf$exempt$find_object(
            request$path
        )
        if (!is.null(match)) return(next_call())
    }

    cookie_name <- fw_env$csrf$cookie_name
    secure <- fw_env$csrf$secure
    safe_methods <- fw_env$csrf$safe_methods
    unsafe_methods <- fw_env$csrf$unsafe_methods

    cookie_token <- request$cookies[[cookie_name]] %||%
        ""
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
        response$set_data(cookie_name, cookie_token)
    }

    if (request$method %in% unsafe_methods) {
        token <- request$get_header("x-csrf-token") %||%
            body$csrf_token
        if (
            is.null(token) ||
                !constant_time_identical(
                    token,
                    cookie_token
                )
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

            if (isTRUE(is_worker)) {
                return(
                    promises::promise(function(resolve, reject) {
                        resolve(
                            list(
                                result = "",
                                continue = plumber2::Break
                            )
                        )

                    })
                )
            } else {
                return(plumber2::Break)
            }
        }
    }

    next_call()
}

csrf_context <- function(api, request, response, fw_env, next_call) {
    if (is.null(request)) {
        return(next_call())
    }

    cookie_name <- fw_env$csrf$cookie_name %||% "csrf"
    token <- request$cookies[[cookie_name]] %||%
        try(response$get_data(cookie_name)) %||%
        ""

    ctx <- get_fw_context()
    ctx$csrf_token <- function() token
    ctx$request <- request

    next_call()
}

ensure_csrf_exempt_router <- function(api) {
    fw_env <- get_freshwater_env(api)
    if (is.null(fw_env$csrf$exempt)) {
        fw_env$csrf$exempt <- waysign::signpost()
        fw_env$csrf$exempt$add_path("/__docs__/*", TRUE)
    }
    fw_env$csrf$exempt
}

csrf_exempt_add <- function(api, pattern) {
  router <- ensure_csrf_exempt_router(api)
  router$add_path(pattern, TRUE)
  invisible(TRUE)
}

constant_time_identical <- function(x, y) {
    if (
        !is.character(x) ||
            !is.character(y) ||
            length(x) != 1L ||
            length(y) != 1L ||
            is.na(x) ||
            is.na(y)
    ) {
        return(FALSE)
    }

    x <- charToRaw(x)
    y <- charToRaw(y)

    if (length(x) != length(y)) {
        return(FALSE)
    }

    bits <- bitwXor(as.integer(x), as.integer(y))
    Reduce(bitwOr, bits, init = 0L) == 0L
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
#' the active request when used within a [template()]. Calling it
#' outside of a `template()` context will result in an error.
#'
#' In most cases, CSRF tokens are inserted automatically
#' for standard form helpers.  Intended for custom forms / custom token
#' placement (meta tags, JS fetch, etc).
#'
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


csrf_tag_handler <- function(block, call, tags, values, env) {
    class(block) <- c("csrf", class(block))

    tag_idx <- which(tags == "csrf")
    tag <- values[[tag_idx]]
    if (!tag %in% c("on", "off", "exempt")) {
        rlang::abort("@csrf must be one of `on`, `off` or `exempt`.")
    }

    if (tag == "off") tag <- "exempt"

    block$csrf_mode <- tag

    block
}

#' @importFrom plumber2 apply_plumber2_block
#' @export
apply_plumber2_block.csrf <- function(
    block,
    api,
    route_name,
    root,
    ...
) {
    NextMethod()
    for (i in seq_along(block$endpoints)) {
        for (path in block$endpoints[[i]]$path) {
            if (identical(block$csrf_mode, "exempt")) {
                csrf_exempt_add(api, paste0(root, path))
            }
        }
    }
    api
}
