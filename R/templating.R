
#' @export
fragment <- function(..., name = NULL) {
    stopifnot(!is.null(name))
    x <- htmltools::as.tags(...)
    x$fragment <- name
    x
}

#' Create a reusable HTML template with optional fragments
#'
#' @description
#' Evaluates code under the `htmltools::withTags()` environment,
#' creating a closure that outputs either the template or
#' specified fragment.
#'
#' @examples
#' page_main <- template(
#'     div(
#'         h1("Dashboard"),
#'         fragment(p("Welcome back"), name="content"),
#'         small("2026")
#'     )
#' )
#' page_main(fragment="content")
#'
#' @param ... content
#' @export
template <- function(...) {
    args <- substitute(...)
    x <- do.call(htmltools::withTags, args = list(args))
    function(fragment = NULL) {
        if (!is.null(fragment)) {
            x <- htmltools::tagQuery(x)$find("*")$selectedTags()
            x <- Filter(x, f = \(x) x$fragment == fragment)
            x <- htmltools::as.tags(x)
            stopifnot(length(x) == 1L)
        }
        x
    }
}
