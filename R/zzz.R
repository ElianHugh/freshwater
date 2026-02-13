.onLoad <- function(libname, pkgname) {
    plumber2::add_plumber2_tag("etag", tag_handler)

    ns <- asNamespace("freshwater")
    if (is.null(ns[["freshwater"]])) {
        ns[["freshwater"]] <- new.env(parent = ns)



    }
}
