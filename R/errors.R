#' @include template.R security.R
utils::globalVariables(c("error", "request"))

#' Freshwater Error Pages
#'
#' Adds request/error hooks to a [plumber2] API so that
#' freshwater can render friendly **HTML error pages** for:
#'  - {**403 Forbidden**} responses
#'  - {**404 Not Found**} responses
#'  - {**500 Internal Server Error**} conditions
#'
#' Custom error page templates can be supplied via the
#' `handler` parameter. These should be freshwater templates
#' created via [template()], and should match the call signatures of
#' the default error templates. See [freshwater_error_templates] for
#' the relevant template signatures required.
#'
#' @param api a [plumber2] api object.
#' @param handlers optional list of named error templates. Supported keys
#' are: "403", "404", "500". If omitted, freshwater installs default templates.
#' @param debug whether the **500** error template should render error
#' messages and stack traces. Defaults to the `fw_debug` plumber2 option,
#' and falls back to `interactive()`.
#'
#' @examples
#' #* @plumber
#' function(api) {
#'  api |>
#'      api_error_pages(debug = TRUE)
#' }
#'
#' @export
#' @seealso [freshwater_error_templates], [enhook_routes]
#' @rdname error_pages
api_error_pages <- function(
    api,
    handlers = NULL,
    debug = plumber2::get_opts("fw_debug", default = interactive())
) {
    if (!requireNamespace("cli", quietly = TRUE)) {
        rlang::abort("cli is required to enable error pages.")
    }

    if (!is.null(handlers)) {
        if (!is.null(handlers[["403"]])) {
            freshwater$missing_handler <- handlers[["403"]]
        }

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

        enhook_routes(
            api,
            hook(
                id = "freshwater::error_page",
                function(api, args, next_call) {
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

                            plumber2::Next
                        }
                    )
                }
            ),
            .where = "prepend"
        )
    })

    plumber2::api_on(api, "request", function(server, id, request, arg_list) {
        response <- request$response %||% NULL

        if (!should_freshwater_handle(request, response)) {
            return(plumber2::Next)
        }

        status <- response$status


        if (!status %in% c(403L, 404L, 500L)) {
            return(plumber2::Next)
        }

        api$trigger(
            "error_code",
            status = status,
            request = request,
            response = response,
            message = NULL
        )

        plumber2::Next
    })

    plumber2::api_on(
        api,
        "error_code",
        function(status, request, response, message) {
            if (!should_freshwater_handle(request, response, message)) {
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
                "403" = {
                    if (use_html) {
                        response$body <- formatter(get_error_template(403)(
                            request
                        ))
                    } else {
                        response$body <- formatter("Forbidden")
                    }
                },
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
                            request,
                            is_debug = debug
                        ))
                        response$set_data("freshwater_handled", TRUE)
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

    invisible(api)
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

should_freshwater_handle <- function(request, response, message = NULL) {
    if (is.null(response) || !is.function(response$set_header)) {
        return(FALSE)
    }

    if (isTRUE(response$get_data("freshwater_handled"))) {
        return(FALSE)
    }

    if (!is.null(message) && inherits(message, "reqres_problem")) {
        return(FALSE)
    }

    TRUE
}

# error templates

get_error_template <- function(error_code) {
    error_code <- as.character(error_code)
    switch(
        error_code,
        "403" = {
            if (is.null(freshwater$forbidden_handler)) {
                freshwater$forbidden_handler <- default_error_403_template
            }
            freshwater$forbidden_handler
        },
        "404" = {
            if (is.null(freshwater$missing_handler)) {
                freshwater$missing_handler <- default_error_404_template
            }
            freshwater$missing_handler
        },
        "500" = {
            if (is.null(freshwater$error_handler)) {
                freshwater$error_handler <- default_error_500_template
            }
            freshwater$error_handler
        },
        NULL
    )
}

#' Error Page Templates
#'
#' freshwater provides a number of default views
#' that are served to HTML clients in the event of common HTTP
#' error codes.
#'
#' @param error the error condition signaled by an error in the server's route handler
#' @param request the [reqres::Request] request object the handler is responding to
#' @param is_debug whether to provide the stack trace and error message to the
#' web client. Although useful during development, it is heavily recommended to
#' set as `FALSE` in production as it can leak sensitive information.
#' @param ... unused
#' @param fragment unused
#'
#' @seealso [api_error_pages], [template]
#' @name freshwater_error_templates
NULL

#' @rdname freshwater_error_templates
#' @export
default_error_500_template <- template(error = NULL, request = NULL, is_debug = FALSE, {
    if (!requireNamespace("cli", quietly = TRUE)) {
        rlang::abort("cli is required to enable error pages.")
    }

    if (isTRUE(is_debug)) {
        raw <- tryCatch(conditionMessage(message), error = function(...) {
            "Unknown error"
        })

        msg_html <- tryCatch(
            cli::ansi_html(raw, escape_reserved = TRUE, csi = "drop"),
            error = function(...) raw
        )

        trace <- error$trace %||% "No trace available"

        trace <- paste0(format(trace), collapse = "\n")

        trace_html <- tryCatch(
            cli::ansi_html(trace, escape_reserved = TRUE, csi = "drop"),
            error = function(...) trace
        )
    }

    css <- tryCatch(
        paste(
            format(cli::ansi_html_style(palette = "iterm-solarized")),
            collapse = "\n"
        ),
        error = function(...) ""
    )

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

#' @rdname freshwater_error_templates
#' @export
default_error_404_template <- template(request = NULL, {
    msg <- if (
        !is.null(request$response$body) && nzchar(request$response$body)
    ) {
        request$response$body
    } else {
        "Server cannot find the requested resource."
    }

    htmltools::tags$html(
        htmltools::tags$head(
            htmltools::tags$title("Not Found")
        ),
        htmltools::tags$body(
            htmltools::tags$h3("freshwater"),
            htmltools::tags$h2("Not Found (Error 404)"),
            htmltools::tags$p(msg)
        )
    )
})

#' @rdname freshwater_error_templates
#' @export
default_error_403_template <- template(request = NULL, {
    msg <- if (!is.null(request$response$body) && nzchar(request$response$body)) {
        request$response$body
    } else {
        ""
    }
    htmltools::tags$html(
        htmltools::tags$head(
            htmltools::tags$title("Forbidden")
        ),
        htmltools::tags$body(
            htmltools::tags$h3("freshwater"),
            htmltools::tags$h2("Forbidden (Error 403)"),
            htmltools::tags$p(msg)
        )
    )
})

