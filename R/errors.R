utils::globalVariables(c("e", "request"))

#' Freshwater Error Pages
#'
#' todo
#'
#' @param api a [plumber2] api object.
#' @param handlers TODO
#'
#' @export
api_error_pages <- function(
    api,
    handlers = NULL
) {
    if (!requireNamespace("cli", quietly = TRUE)) {
        rlang::abort("cli is required to enable error pages.")
    }

    if (!is.null(handlers)) {
        if (!is.null(handlers[["404"]])) {
            freshwater$missing_handler <- handlers[["404"]]
        }

        if (!is.null(handlers[["500"]])) {
            freshwater$error_handler <- handlers[["500"]]
        }
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
                    if (!is.null(response)) {
                        response$status <- 500L
                    }

                    api$trigger(
                        "error_code",
                        status = 500L,
                        request = request,
                        response = response,
                        message = e
                    )

                    plumber2::Break
                }
            )
        })
    })

    plumber2::api_on(api, "request", function(server, id, request, arg_list) {
        response <- request$response %||% NULL

        if (is.null(response) || !is.function(response$set_header)) {
            return(plumber2::Next)
        }

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
            if (is.null(response) || !is.function(response$set_header)) {
                return(plumber2::Next)
            }

            if (is.null(status) || !length(status)) {
                return(plumber2::Next)
            }

            status <- as.character(status)
            use_html <- wants_html(request)


            if (use_html) {
                response$set_header(
                    "Content-Type",
                    "text/html; charset=utf-8"
                )
            } else {
                response$set_header(
                    "Content-Type",
                    "text/plain; charset=utf-8"
                )
            }

            formatter <- if (use_html) {
                plumber2::get_serializers("html")[[1L]]
            } else {
                plumber2::get_serializers("text")[[1L]]
            }

            switch(
                status,
                "404" = {
                    if (use_html) {
                        response$body <- formatter(get_error_template(404)(
                            request
                        ))
                    } else {
                        response$body <- formatter("Not Found")
                    }
                },
                "500" = {
                    if (use_html) {
                        response$body <- formatter(get_error_template(500)(
                            message,
                            request
                        ))
                    } else {
                        response$body <- formatter("Internal Server Error")
                    }
                },
                rlang::abort(sprintf(
                    "Unhandled error code? Got {%s}.",
                    status
                ))
            )

            plumber2::Break
        }
    )

    api
}

wants_html <- function(request) {
    accept <- ""
    if (!is.null(request) && is.function(request$get_header)) {
        accept <- request$get_header("accept") %||% ""
    }
    any(
        grepl("text/html", accept, fixed = TRUE)
    )
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

        # if (inherits(e, "freshwater_template_error")) {

        # } else {
        #     # trace <-
        #     # todo
        # }

        trace <- e$trace %||% "No trace available"

        trace <- paste0(format(trace), collapse = "\n")



        trace_html <- tryCatch(
            cli::ansi_html(trace, escape_reserved = TRUE, csi = "drop"),
            error = function(...) trace
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
                    htmltools::div(
                        htmltools::tags$details(
                            htmltools::tags$summary("Error message"),
                            open = NA,
                            htmltools::tags$pre(htmltools::HTML(msg_html))
                        ),
                        htmltools::tags$details(
                            htmltools::tags$summary("Stack trace"),
                            htmltools::tags$pre(htmltools::HTML(trace_html))
                        )
                    )

                }
            )
        )
    })
}

default_error_404_template <- function() {
    template(request = NULL, {
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
