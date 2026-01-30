.onLoad <- function(libname, pkgname) {
    plumber2::add_plumber2_tag("etag", tag_handler)
}
