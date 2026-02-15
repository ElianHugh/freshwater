utils::globalVariables(c("e", "request"))

#' Freshwater Error Pages
#'
#' todo
#'
#' @export
#' @param api a [plumber2] api object.
api_error_pages <- function(api) {
    if (!requireNamespace("cli", quietly = TRUE)) {
        rlang::abort("cli is required to enable error pages.")
    }

    if (isTRUE(attr(api, "error_pages_installed", exact = TRUE))) {
        return(api)
    }

    attr(api, "error_pages_installed") <- TRUE

    plumber2::api_on(api, "start", function(...) {
        api$trigger("freshwater_error_pages")
    })

    plumber2::api_on(api, "freshwater_error_pages", function(...) {
        if (isTRUE(attr(api, "error_hooked", exact = TRUE))) {
            return(invisible(NULL))
        }
        attr(api, "error_hooked") <- TRUE
        enhook_routes(api, error_500_page_hook)
    })

    plumber2::api_on(api, "request", error_404_page_hook)
}

error_500_page_hook <- function(args, next_call) {
    response <- args$response %||% NULL
    request <- args$request %||% NULL

    tryCatch(
        next_call(),
        error = function(e) {
            if (is.null(response)) {
                stop(e)
            }

            accept <- ""
            if (!is.null(request) && is.function(request$get_header)) {
                accept <- request$get_header("accept") %||% ""
            }

            wants_html <- any(grepl("text/html", accept, fixed = TRUE))

            response$status <- 500L

            if (wants_html) {
                response$set_header("Content-Type", "text/html; charset=utf-8")

                response$body <- response$formatter(get_error_500_template()(
                    e,
                    request
                ))
            } else {
                response$set_header("Content-Type", "text/plain; charset=utf-8")
                response$body <- response$formatter(
                    paste0(
                        "Internal Server Error"
                    )
                )
            }

            plumber2::Break
        }
    )
}

get_error_500_template <- function() {
    if (is.null(freshwater$error_handler)) {
        freshwater$error_handler <- default_error_500_template()
    }
    freshwater$error_handler
}

default_error_500_template <- function() {
    if (!requireNamespace("cli", quietly = TRUE)) {
        rlang::abort("cli is required to enable error pages.")
    }

    template(e = NULL, request = NULL, {
        raw <- tryCatch(conditionMessage(e), error = function(...) {
            "Unknown error"
        })
        msg_html <- tryCatch(
            cli::ansi_html(raw, escape_reserved = TRUE, csi = "drop"),
            error = function(...) raw
        )

        css <- tryCatch(
            paste(
                format(cli::ansi_html_style(palette = "iterm-solarized")),
                collapse = "\n"
            ),
            error = function(...) ""
        )

        is_debug <- TRUE #todo

        # fully qualifying the tags to pacify R CMD

        htmltools::tags$html(
            htmltools::tags$head(
                htmltools::tags$title("Server error"),
                if (nzchar(css)) htmltools::tags$style(css)
            ),
            htmltools::tags$body(
                htmltools::tags$h3("freshwater"),
                htmltools::tags$h2("Something went wrong (Error 500)"),
                htmltools::tags$p(
                    "The server hit an error while processing your request."
                ),
                if (isTRUE(is_debug)) {
                    htmltools::tags$details(
                        open = NA,
                        htmltools::tags$pre(htmltools::HTML(msg_html))
                    )
                }
            )
        )
    })
}


error_404_page_hook <- function(server, id, request, arg_list) {
    response <- request$response
    status <- response$status

    if (!identical(status, 404L)) {
        return(plumber2::Next)
    }

    accept <- ""
    if (!is.null(request) && is.function(request$get_header)) {
        accept <- request$get_header("accept") %||% ""
    }

    wants_html <- any(grepl("text/html", accept, fixed = TRUE))
    response$set_header("Content-Type", "text/html; charset=utf-8")
    formatter <- plumber2::get_serializers("html")[[1L]]
    response$body <- formatter(get_error_404_template()())

    plumber2::Break
}

get_error_404_template <- function() {
    if (is.null(freshwater$error_404_handler)) {
        freshwater$error_404_handler <- default_error_404_template()
    }
    freshwater$error_404_handler
}

default_error_404_template <- function() {
    template({
        htmltools::tags$html(
            htmltools::tags$head(
                htmltools::tags$title("Server error")
            ),
            htmltools::tags$body(
                htmltools::tags$h3("freshwater"),
                htmltools::tags$h2("Not Found (Error 404)"),
                htmltools::tags$p(
                    "Server cannot find the requested resource."
                )
            )
        )
    })
}