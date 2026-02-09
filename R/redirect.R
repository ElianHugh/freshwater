
#' Redirect to another resource
#'
#' Sends a 303 response and halts request processing.
#' Client is redirected to the given location.
#'
#' This is commonly used in Post/Redirect/GET (PRG) setups
#' to redirect clients to a new page following
#' form submissions.
#'
#' @param response Plumber2 response object
#' @param location path or url to redirect to
#'
#' @examples
#' #* @get /
#' function(response) {
#'     print("Hello!")
#'     redirect(response, "/foo")
#' }
#' @export
redirect <- function(response, location) {
    response$status <- 303L
    response$set_header("Location", location)
    plumber2::Break
}