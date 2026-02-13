
#' Redirect to another resource
#'
#' If `after` is NULL, sends a 303 response and halts request processing.
#' Client is redirected to the given location. This is commonly used in
#' Post/Redirect/GET (PRG) setups to redirect clients to a new page following
#' form submissions.
#'
#' If after is a numeric, a "Refresh" header is attached to the response,
#' instructing the browser to navigate to `location` after the specified number
#' of seconds.
#'
#' @param response [reqres::Response] object
#' @param location path or url to redirect to
#' @param after optional number of seconds to wait before redirection
#'
#' @details
#' The delayed redirect uses the non-standard "Refresh" HTTP header which is
#' widely supported by browsers but is not part of the official HTTP specification.
#' It should not be relied on for API & non-browser clients.
#'
#' See also:
#' - <https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Redirections>
#' - <https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Refresh>
#'
#' @return
#' - [plumber2::Break] when issuing an immediate redirect.
#' - [plumber2::Next] when issuing a delayed navigation.
#'
#' @examples
#' # Immediate redirect (PRG pattern)
#' #* @get /
#' function(response) {
#'     print("Hello!")
#'     redirect(response, "/foo")
#' }
#'
#' # Delayed redirect after rendering content
#' #* @get /count/<n>
#' function(n, response) {
#'   redirect(response, "/", after = 1)
#'   paste("n =", n)
#' }
#' @export
redirect <- function(response, location, after = NULL) {
    location <- gsub("[\r\n]", "", location)
    if (is.null(after)) {
        response$status <- 303L
        response$set_header("Location", location)
        return(plumber2::Break)
    } else {
        after <- as.integer(after)
        !is.na(after) ||
            rlang::abort(
                c(
                    "`after` should be a number greater than 0L.",
                    sprintf("Got `%s`", after)
                )
            )
        response$set_header(
            "Refresh",
            sprintf("%s; url=%s", after, location)
        )
    }
    plumber2::Next
}
