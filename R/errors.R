#' @export
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
        enhook_routes(api, error_page_hook)
    })
}

error_page_hook <- function(args, next_call) {
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

                response$body <- response$formatter(get_error_template()(e, request))
            } else {
                response$set_header("Content-Type", "text/plain; charset=utf-8")
                response$body <- response$formatter(
                    paste0(
                        "Internal Server Error\n\n",
                        conditionMessage(e)
                    )
                )
            }

            plumber2::Break
        }
    )
}

default_error_template <- function() {
    template(e, request = NULL, {
        msg <- cli::ansi_html(conditionMessage(e))
        css <- paste(format(cli::ansi_html_style(palette="iterm-solarized")), collapse = "\n")
        is_debug <- TRUE #todo

        html(
            head(
                title("Server error"),
                style(css),
            ),
            body(
                h3("freshwater"),
                h2("Something went wrong"),
                p("The server hit an error while processing your request."),
                if (isTRUE(is_debug)) {
                    details(
                        open = NA,
                        pre(htmltools::HTML(msg))
                    )
                }
            )
        )
    })
}

get_error_template <- function() {
  if (is.null(freshwater$error_handler)) {
    freshwater$error_handler <- default_error_template()
  }
  freshwater$error_handler
}
