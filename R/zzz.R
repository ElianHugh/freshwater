.onLoad <- function(libname, pkgname) {
    plumber2::add_plumber2_tag("etag", cget_tag_handler)
    plumber2::add_plumber2_tag("csrf", csrf_tag_handler)
    ns <- asNamespace("freshwater")
    if (is.null(ns[["freshwater"]])) {
        ns[["freshwater"]] <- new.env(parent = ns)
    }
    ensure_cache_state()
}
