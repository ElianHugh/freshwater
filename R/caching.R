#' Conditional GET
#'
#' Creates a conditional GET handler for a specific
#' HTTP path, using a supplied etag function
#' for the current representation. If the server's ETag and
#' client's 'If-None-Match' headers match, a `304 Not Modified` response
#' is sent, short-circuiting downstream handlers. This reduces
#' the need to recompute responses for paths where the data
#' has not changed since the last client request.
#'
#' See <https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Conditional_requests> for more information.
#'
#' @details
#' # Annotation Reference
#'
#' The etag function is specified by `@etag fn` where `fn` is the function name.
#' Functions can also be defined in-line
#' like `@etag \() x + 1`.
#'
#' ```r
#' increment_x <- \() {
#'     x <<- x + 1L
#'     later::later(increment_x, delay = 10L)
#' }
#' x <- 1L
#' increment_x()
#' #* @get /
#' #* @etag \() x
#' function() {
#'  x
#' }
#' ```
#'
#' @param api a [plumber2] api object.
#' @param path the path to short circuit.
#' @param etag_fn a function that takes no arguments and returns a single value used to derive the ETag.
#' @export
api_cget <- function(api, path, etag_fn) {
    handler <- function(request, response) {
        inm <- request$get_header("If-None-Match")

        etag <- new_etag(etag_fn())

        response$set_header("ETag", etag)

        if (inm_match(inm, etag)) {
            response$status <- 304L
            plumber2::Break
        } else {
            plumber2::Next
        }
    }

    plumber2::api_get_header(api, path = path, handler = handler)

    api
}

#' @importFrom plumber2 apply_plumber2_block
#' @export
apply_plumber2_block.etag <- function(
    block,
    api,
    route_name,
    root,
    ...
) {
    NextMethod()
    for (i in seq_along(block$endpoints)) {
        for (path in block$endpoints[[i]]$path) {
            api <- api_cget(
                api,
                etag_fn = block$etag_fn,
                path = paste0(root, path)
            )
        }
    }
    api
}

tag_handler <- function(block, call, tags, values, env) {
    class(block) <- c("etag", class(block))
    stopifnot("get" %in% tags)

    tag_idx <- which(tags == "etag")
    # tag function should be a function that is called without any arguments
    # and returns a single value that can be interpreted as a string
    tag_fn <- eval(parse(text = values[[tag_idx]] %||% ""), envir = env)

    stopifnot(is.function(tag_fn))
    block$etag_fn <- tag_fn
    block
}


new_etag <- function(x) {
    stopifnot(length(x) == 1L, !is.null(x), !is.na(x))
    sprintf('W/"%s"', as.character(x))
}

strip_weak_prefix <- function(x) {
    reg <- regexec('^(?:W/)?(".*")$', x)
    res <- regmatches(x, reg)[[1]]
    if (length(res) == 0) {
        return(NA_character_)
    }
    res[[2]]
}

inm_match <- function(inm, etag) {
    inm <- trimws(inm %||% "")

    etag <- strip_weak_prefix(etag)

    if (is.na(etag)) {
        return(FALSE)
    }
    if (identical(inm, "")) {
        return(FALSE)
    }
    if (identical(inm, "*")) {
        return(TRUE)
    }

    candidates <- trimws(strsplit(inm, ",", fixed = TRUE)[[1L]])
    candidates <- candidates[nzchar(candidates)]
    candidates <- vapply(candidates, strip_weak_prefix, character(1L))

    any(!is.na(candidates) & candidates == etag)
}
