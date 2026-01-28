#' Create a reusable HTML template
#'
#' @description
#' Evaluates code under the `htmltools::withTags()` environment,
#' creating a closure that outputs either the template or
#' specified fragment.
#'
#' The closure can also accept slots, specified as named or bare symbols prior
#' to the template expression. This allows for passing in other templates or HTML
#' tags after template creation.
#'
#' @examples
#' # Example Fragment Usage
#' page_main <- template(
#'     {
#'         div(
#'             h1("Dashboard"),
#'             fragment(p("Welcome back"), name = "content"),
#'             small("2026")
#'         )
#'     }
#' )
#'
#' page_main(fragment="content")
#'
#' # Template slots
#'
#' details <- template(name, age, {
#'     nm <- sprintf("Hello, my name is: %s", name)
#'     old <- sprintf("I am %s years old.", age)
#'     div(
#'         p(nm),
#'         p(old)
#'     )
#' })
#'
#' details("Jim", 30)
#'
#' # Templates and fragments can also be combined
#'
#' card <- template(
#'     ttl, foot = NULL, {
#'         div(
#'             h2(ttl),
#'             fragment(div("Card body"), name="body"),
#'             if (!is.null(foot)) {
#'                 fragment(
#'                     div(foot),
#'                     name = "footer"
#'                 )
#'             }
#'         )
#'     }
#' )
#' card("Card Title")
#' card("Card Title", fragment="body")
#' card("Card Title", "Footer text", fragment = "footer")
#'
#' @param ... content
#' @param .envir the environment in which to evaluate the template
#' @rdname templating
#' @export
template <- function(..., .envir = parent.frame()) {
    dots <- as.list(substitute(list(...)))[-1]
    body_idx <- which(unlist(lapply(dots, \(x) inherits(x, "{"))))

    if (length(body_idx) != 1L) {
        stop()
    }

    body_expr <- dots[body_idx][[1L]]

    param_exprs <- dots[-body_idx]
    param_names <- names(param_exprs) %||% character(length(param_exprs))

    for (i in seq_along(param_exprs)) {
        if (identical(param_names[i], "") || is.null(param_names[i])) {
            if (!is.symbol(param_exprs[[i]])) {
                stop(
                    sprintf(
                        "Unnamed parameters must be bare symbols: %s",
                        deparse(param_exprs[[i]])
                    )
                )
            }

            param_names[[i]] <- as.character(param_exprs[[i]])
            param_exprs[[i]] <- quote(expr = )
        }
    }

    names(param_exprs) <- param_names

    if ("fragment" %in% names(param_exprs)) {
        stop("Duplicate `fragment` parameter found. `fragment` is a reserved template argument.")
    }

    formals_pl <- as.pairlist(c(param_exprs, alist(...=), list(fragment = NULL)))

    f_body <- substitute(
        {
            if (!is.null(fragment)) {
                x <- walk_nodes(x, fragment)
                stopifnot("Could not find fragment" = !is.null(x))
            }
            x
        },
        list(x = body_expr)
    )

    e <- new.env(parent = .envir)
    list2env(as.list(htmltools::tags), envir = e)

    fn <- eval(call("function", formals_pl, f_body), e)
    class(fn) <- c("freshwater_template", class(fn))
    attr(fn, "template_body") <- body_expr
    attr(fn, "template_params") <- param_exprs
    attr(fn, "template_env") <- .envir

    fn
}

#' @rdname templating
#' @param name the name of the fragment
#' @export
fragment <- function(..., name = NULL) {
    stopifnot(!is.null(name))
    x <- htmltools::as.tags(...)
    x$fragment <- name
    x
}

#' @exportS3Method
print.freshwater_template <- function(x, ...) {
    params <- attr(x, "template_params")
    body <- attr(x, "template_body")
    e <- attr(x, "template_env")

    params_string <- character(0)

    if (length(params)) {
        nms <- names(params)
        for (i in seq_along(params)) {
            nm <- nms[[i]]
            param <- params[[i]]

            if (inherits(params[[nm]], "name")) {
                params_string <- c(params_string, nm)
            } else {
                params_string <- c(params_string, paste0(nm, " = ", deparse(param)))
            }
        }
    }

    out <- "template(%s)\n%s\n%s"

    out <- sprintf(
        out,
        paste0(
            c(params_string, "...", "fragment = NULL"), collapse=", "),
        paste0(deparse(body), collapse="\n"),
        format(e)
    )

    cat(out)
}

walk_nodes <- function(tag, name) {
    found <- NULL
    walk <- function(x) {
        if (!is.null(found)) return()

        if (inherits(x, "shiny.tag")) {
            if (!is.null(x$fragment) && identical(x$fragment, name)) {
                found <<- x
                return()
            }

            for (i in seq_along(x$children)) {
                walk(x$children[[i]])
                if (!is.null(found)) return()
            }
        } else if (inherits(x, "shiny.tag.list")) {
            for (child in x) {
                walk(child)
                if (!is.null(found)) return()
            }
        }
    }

    walk(tag)
    found
}
