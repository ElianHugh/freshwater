
new_etag <- function(x) {
    stopifnot(length(x) == 1L, !is.null(x), !is.na(x))
    sprintf('W/"%s"', as.character(x))
}

inm_match <- function(inm, etag) {
    inm <- trimws(inm %||% "")

    if (identical(inm, "")) return(FALSE)
    if (identical(inm, "*")) return(TRUE)

    candidates <- trimws(strsplit(inm, ",", fixed = TRUE)[[1L]])
    etag %in% candidates
}

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

plumber2::add_plumber2_tag("etag", function(block, call, tags, values, env) {
    class(block) <- c("etag", class(block))
    stopifnot("get" %in% tags)

    tag_idx <- which(tags == "etag")
    # tag function should be a function that is called without any arguments
    # and returns a single value that can be interpreted as a string
    tag_fn <- eval(parse(text=values[[tag_idx]] %||% ""), envir = env)

    stopifnot(is.function(tag_fn))
    block$etag_fn <- tag_fn
    block
})

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