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
#' @export
register_html_serialiser <- function() {
    eval(substitute(
        {
            plumber2::register_serializer(
                "html",
                function(...) {
                    function(x) {
                        if (inherits(x, c("shiny.tag", "shiny.tag.list"))) {
                            htmltools::doRenderTags(x) |>
                                as.character()
                        } else {
                            old_serialiser(x)
                        }
                    }
                },
                mime_type = "text/html"
            )
        },
        list(old_serialiser = plumber2::get_serializers("html")[[1L]])
    ))
}


