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
#' `handlers` parameter. These should be freshwater templates
#' created via [template()], and should match the call signatures of
#' the default error templates. See [freshwater_error_templates] for
#' the relevant template signatures required.
#'
#' This middleware installs freshwater request context.
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
#' @seealso [freshwater_error_templates], [api_hooks]
#' @rdname error_pages
api_error_pages <- function(
    api,
    handlers = NULL,
    debug = plumber2::get_opts("fw_debug", default = interactive())
) {
    fw_env <- get_freshwater_env(api)

    if (!is.null(handlers)) {
        if (!is.null(handlers[["403"]])) {
            fw_env$error_pages$handlers[["403"]] <- handlers[["403"]]
        }

        if (!is.null(handlers[["404"]])) {
            fw_env$error_pages$handlers[["404"]] <- handlers[["404"]]
        }

        if (!is.null(handlers[["500"]])) {
             fw_env$error_pages$handlers[["500"]] <- handlers[["500"]]
        }
    }


    if (isTRUE(fw_env$error_pages$installed)) {
        return(api)
    }

    fw_env$error_pages$installed <- TRUE
    fw_env$error_pages$debug <- debug

    api <- api_context(api)

    api <- api_hooks(
        api,
        hook(
            id = "freshwater::error_pages",
            function(api, args, next_call) {
                response <- args$response %||% NULL
                request <- args$request %||% NULL
                tryCatch(
                    expr = {
                        res <- next_call()

                         if (!response$status %in% c(403L, 404L, 500L)) {
                            return(res)
                        }

                        if (!should_freshwater_handle(request, response)) {
                            return(res)
                        }

                        api$trigger(
                            "error_code",
                            status = response$status,
                            request = request,
                            response = response,
                            message = NULL
                        )

                        plumber2::Next

                    },
                    error = function(e) {
                        if (!should_freshwater_handle(request, response, e)) {
                            return(plumber2::Next)
                        }

                        if (!inherits(e, "reqres_problem")) {
                            response$status <- 500L
                        } else {
                            response$status <- e$status
                        }

                        api$trigger(
                            "error_code",
                            status = response$status,
                            request = request,
                            response = response,
                            message = e
                        )

                        plumber2::Next
                    }
                )
            }
        )
    )

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

            # etags should not happen on 500s
            status <- as.character(status)
            use_html <- wants_html(request)

            if (status != "404") {
                response$remove_header("ETag")
            }

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
                        response$body <- formatter(get_error_template(api, 403)(
                            message,
                            request
                        ))
                        response$set_data("freshwater_handled", TRUE)
                    } else {
                        response$body <- formatter("Forbidden")
                    }
                },
                "404" = {
                    if (use_html) {
                        response$body <- formatter(get_error_template(api, 404)(
                            message,
                            request
                        ))
                    } else {
                        response$body <- formatter("Not Found")
                    }
                },
                "500" = {
                    if (use_html) {
                        response$body <- formatter(get_error_template(api, 500)(
                            message,
                            request,
                            is_debug = fw_env$error_pages$debug
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
            plumber2::Next
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

    TRUE
}

# error templates

get_error_template <- function(api, error_code) {
    fw_env <- get_freshwater_env(api)
    error_code <- as.character(error_code)

    switch(
        error_code,
        "403" = {
            if (is.null(fw_env$error_pages$handlers[["403"]])) {
                fw_env$error_pages$handlers[["403"]] <- default_error_403_template
            }
            fw_env$error_pages$handlers[["403"]]
        },
        "404" = {
            if (is.null(fw_env$error_pages$handlers[["404"]])) {
                fw_env$error_pages$handlers[["404"]] <- default_error_404_template
            }
            fw_env$error_pages$handlers[["404"]]
        },
        "500" = {
            if (is.null(fw_env$error_pages$handlers[["500"]])) {
                fw_env$error_pages$handlers[["500"]] <- default_error_500_template
            }
            fw_env$error_pages$handlers[["500"]]
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


# fully qualifying the tags to pacify R CMD
error_page <- template(
    title,
    heading = title,
    code = 500L,
    sub = NULL,
    head_extra = NULL,
    {
        css_path <- system.file(
            "assets",
            "error.css",
            package = "freshwater"
        )

        document(
            htmltools::tags$head(
                htmltools::tags$title("Server error"),
                if (!is.null(head_extra)) head_extra,
                if (nzchar(css_path) && file.exists(css_path)) {
                    htmltools::includeCSS(css_path)
                }
            ),
            htmltools::tags$body(
                htmltools::tags$div(
                    class = "fw-card",
                    htmltools::tags$h3("freshwater", class = "fw-brand"),
                    htmltools::tags$h2(
                        class = "fw-title",
                        heading,
                        htmltools::tags$span(
                            as.character(code),
                            class = "fw-badge"
                        )
                    ),
                    if (!is.null(sub)) htmltools::tags$p(sub, class = "fw-sub"),
                    ...
                )
            )
        )
    }
)


#' @rdname freshwater_error_templates
#' @export
default_error_500_template <- template(error = NULL, request = NULL, is_debug = FALSE, {

    if (isTRUE(is_debug)) {
        old <- options(cli.num_colors = 256)
        on.exit(options(old), add = TRUE)

        raw <- tryCatch(
            conditionMessage(error),
            error = function(...)  "Unknown error"
        )

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

    error_page(
        title = "Server error",
        heading = "Something went wrong",
        code = 500L,
        sub = "The server hit an error while processing your request.",
        head_extra = htmltools::tags$style(css),
        if (isTRUE(is_debug)) {
            htmltools::tags$div(
                class = "fw-error-block",
                htmltools::tags$details(
                    summary("Error message"),
                    open = NA,
                    htmltools::tags$pre(htmltools::HTML(msg_html))
                ),
                htmltools::tags$details(summary("Stack trace"), htmltools::tags$pre(htmltools::HTML(trace_html)))
            )
        }
    )
})

#' @rdname freshwater_error_templates
#' @export
default_error_404_template <- template(error = NULL, request = NULL, {
    msg <- "Server cannot find the requested resource."
    msg <- if (!is.null(error) && inherits(error, "reqres_problem")) {
        error$detail %||% msg
    } else {
        msg
    }

    error_page(
        title = "Not Found",
        code = 404L,
        sub = msg
    )
})

#' @rdname freshwater_error_templates
#' @export
default_error_403_template <- template(error = NULL, request = NULL, {
    msg <- "You don't have permission to access this resource."
    msg <- if (!is.null(error) && inherits(error, "reqres_problem")) {
        error$detail %||% msg
    # } else if (!is.null(request$response$body) && nzchar(request$response$body)) {
    #     request$response$body
    } else {
       msg
    }

    error_page(
        title = "Forbidden",
        code = 403L,
        sub = msg
    )
})

