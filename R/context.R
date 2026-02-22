#' @include handler_hooks.R

#' @export
api_freshwater <- function(api, csrf = TRUE, error_pages = TRUE, ...) {
    register_html_serialiser()

    api <- api_context(api)

    if (csrf) api <- api_csrf(api, ...)
    if (error_pages) api <- api_error_pages(api, ...)

    invisible(api)
}

# todo
readonly_view <- function(env) {}

#' @keywords internal
api_context <- function(api) {
    if (isTRUE(attr(api, "context_installed", exact = TRUE))) {
        return(api)
    }

    attr(api, "context_installed") <- TRUE

    plumber2::api_on(api, "start", function(...) {
        api$trigger("freshwater_context")
    })

    plumber2::api_on(api, "freshwater_context", function(...) {
        if (isTRUE(attr(api, "context_hooked", exact = TRUE))) {
            return(invisible(NULL))
        }
        attr(api, "context_hooked") <- TRUE
        enhook_routes(
            api,
            hook("freshwater::context", function(api, args, next_call) {
                request <- args$request
                response <- request$response

                if (is.null(request)) {
                    return(next_call())
                }

                ctx <- new.env(parent = emptyenv())
                ctx$request <- request
                old <- set_fw_context(ctx)
                on.exit(set_fw_context(old), add = TRUE)

                next_call()
            }),
            .where = "prepend"
        )
    })

    invisible(api)
}

set_fw_context <- function(ctx) {
    old <- freshwater$request_context %||% NULL
    freshwater$request_context <- ctx
    old
}

get_fw_context <- function() {
    freshwater$request_context %||% NULL
}

current_root <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) return("")
    ctx$request$root
}

current_path <- function() {
    ctx <- get_fw_context()
    if (is.null(ctx)) return("")
    ctx$request$path
}