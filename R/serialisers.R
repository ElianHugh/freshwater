#' Register HTML Serialiser
#'
#' Registers an HTML serialiser for [plumber2]
#' that renders shiny tags and taglists via
#' [htmltools::doRenderTags()]. This is preferrable
#' over [htmltools::renderTags()] as often we want to
#' be able to emit head tags which [htmltools] attempts
#' to hide for shiny usecases.
#'
#' The freshwater serialiser safely falls back
#' to the default plumber2 implementation for other
#' classes of inputs.
#'
#' Specifically, if an input inherits `shiny.tag`,
#' `shiny.tag.list` or `html`, it will be serialised via
#' [htmltools::as.tags()] and [htmltools::doRenderTags()].
#' Otherwise, the default HTML serialiser will be used. If missing,
#' the input will be coerced via [as.character()].
#'
#' **Registration affects global process state, not just the current API process.**
#'
#' @param force bypass checks and re-register
#' the freshwater serialiser
#'
#' @export
register_html_serialiser <- function(force = FALSE) {
    # i believe this is a global thing(?) so we dont register it per api
    if (!force && isTRUE(freshwater$serialiser_registered)) {
        return(invisible(NULL))
    }

    if (is.null(freshwater$old_html_serialiser)) {
        serialisers <- plumber2::get_serializers("html")
        if (!(length(serialisers))) {
            rlang::abort("No existing html serialisers found to wrap.")
        }
        freshwater$old_html_serialiser <- serialisers[[1L]]
    }

    plumber2::register_serializer(
        "html",
        function(...) {
            function(x) {
                if (inherits(x, c("shiny.tag", "shiny.tag.list", "html"))) {
                    htmltools::as.tags(x) |>
                        htmltools::doRenderTags() |>
                        as.character()
                } else if (is.function(freshwater$old_html_serialiser)) {
                    freshwater$old_html_serialiser(x)
                } else {
                    as.character(x)
                }
            }
        },
        mime_type = "text/html",
        default = TRUE
    )

    freshwater$serialiser_registered <- TRUE

    invisible(NULL)
}
