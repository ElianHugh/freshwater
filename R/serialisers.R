#' @export
register_html_serialiser <- function() {
    old_serialiser <- plumber2::get_serializers("html")

    plumber2::register_serializer(
        "html",
        function(...) {
            function(x) {
                if (inherits(x, c("shiny.tag", "shiny.tag.list"))) {
                    doRenderTags(x) |>
                        as.character()
                } else {
                    old_serialiser(x)
                }
            }
        },
        mime_type = "text/html"
    )
}
