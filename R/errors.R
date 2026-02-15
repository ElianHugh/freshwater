utils::globalVariables(c("e", "request"))

#' Freshwater Error Pages
#'
#' todo
#'
#' @export
#' @param api a [plumber2] api object.
api_error_pages <- function(api, server_error = NULL, not_found = NULL) {
    if (!requireNamespace("cli", quietly = TRUE)) {
        rlang::abort("cli is required to enable error pages.")
    }

    if (!is.null(server_error)) {
        freshwater$error_handler <- server_error
    }

    if (!is.null(not_found)) {
        freshwater$missing_handler <- not_found
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
        enhook_routes(api, function(api, args, next_call) {
            response <- args$response %||% NULL
            request <- args$request %||% NULL

            tryCatch(
                next_call(),
                error = function(e) {
                    api$trigger(
                        "error_code",
                        status = 500L,
                        request = request,
                        response = response,
                        message = e
                    )
                    plumber2::Next
                }
            )
        })
    })

    plumber2::api_on(api, "request", function(server, id, request, arg_list) {
        response <- request$response
        status <- response$status

        if (!status %in% c(404L, 500L)) {
            return(plumber2::Next)
        }

        api$trigger(
            "error_code",
            status = status,
            request = request,
            response = response,
            message = NULL
        )

        plumber2::Break
    })

    plumber2::api_on(
        api,
        "error_code",
        function(status, request, response, message) {
            status <- as.character(status)
            if (length(status)) {
                switch(
                    status,
                    "404" = error_404_page_hook(request, response),
                    "500" = error_500_page(request, response, message),
                    rlang::abort(sprintf(
                        "Unhandled error code? Got {%s}.",
                        status
                    ))
                )
            }
        }
    )

    api
}

error_500_page <- function(request = NULL, response = NULL, error) {
    if (is.null(response)) {
        stop(error)
    }

    response$status <- 500L

    if (wants_html(request)) {
        response$set_header("Content-Type", "text/html; charset=utf-8")
        formatter <- plumber2::get_serializers("html")[[1L]]
        response$body <- formatter(get_error_template(500)(error, request))
    } else {
        response$set_header("Content-Type", "text/plain; charset=utf-8")
        formatter <- plumber2::get_serializers("text")[[1L]]
        response$body <- formatter("Internal Server Error")
    }

    plumber2::Break
}

error_404_page_hook <- function(request, response) {

    if (wants_html(request)) {
        response$set_header("Content-Type", "text/html; charset=utf-8")
        formatter <- plumber2::get_serializers("html")[[1L]]
        response$body <- formatter(get_error_template(404)(request))
    } else {
        response$set_header("Content-Type", "text/plain; charset=utf-8")
        formatter <- plumber2::get_serializers("text")[[1L]]
        response$body <- formatter("Not Found")
    }

    plumber2::Break
}

# error templates

get_error_template <- function(error_code) {
    error_code <- as.character(error_code)
    switch(
        error_code,
        "404" = {
            if (is.null(freshwater$missing_handler)) {
                freshwater$missing_handler <- default_error_404_template()
            }
            freshwater$missing_handler
        },
        "500" = {
            if (is.null(freshwater$error_handler)) {
                freshwater$error_handler <- default_error_500_template()
            }
            freshwater$error_handler
        },
        NULL
    )
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

default_error_404_template <- function() {
    template(request, {
        htmltools::tags$html(
            htmltools::tags$head(
                htmltools::tags$title("Not Found")
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

wants_html <- function(request) {
    accept <- ""
    if (!is.null(request) && is.function(request$get_header)) {
        accept <- request$get_header("accept") %||% ""
    }
    any(
        grepl("text/html", accept, fixed = TRUE) |
            grepl("*/*", accept, fixed = TRUE)
    )
}
