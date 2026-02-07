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
#' @param force bypass checks and re-register
#' the freshwater serialiser
#'
#' @export
register_html_serialiser <- function(force = FALSE) {
    if (!force && isTRUE(freshwater$serialiser_registered)) {
        rlang::warn(
            "freshwater html serialiser has already been registered. Skipping re-registration. Use `force=TRUE` to bypass checks."
        )
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
                if (inherits(x, c("shiny.tag", "shiny.tag.list"))) {
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
        mime_type = "text/html"
    )

    freshwater$serialiser_registered <- TRUE

    invisible(NULL)
}
