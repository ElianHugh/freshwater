#' Create a reusable HTML template
#'
#' @description
#' `template()` is a function factory that captures a template expression and
#' returns a callable HTML renderer. The expression is evaluated
#' under the [htmltools::withTags()] environment, so tag functions such as
#' `div()` or `p()` are available.
#'
#' Templates may define:
#' - **parameters**: symbols or named defaults which are used as arguments
#' to the renderer
#' - **content injection**: if the template uses `...`, the renderer
#'  `...`  passes them to the containing HTML nodes
#' defined in the template.
#' - **fragments**: named subtemplates that can be optionally extracted from
#' the template upon rendering by supplying `fragment = "name"`. Fragment names
#' are required.
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
#' # Dots (content injection)
#' layout <- template({
#'     htmltools::tagList(
#'         head(meta(title = "foo")),
#'         body(...)
#'     )
#' })
#'
#' layout(htmltools::div("content"))
#'
#' @param ... template definition. Provide zero or more parameters, followed by a
#' single braced expression.
#' @param .envir the environment in which to evaluate the template
#' @return function of class `template` with interface `fn(<declared params>, ..., fragment = NULL)`
#' @rdname templating
#' @export
template <- function(..., .envir = parent.frame()) {
    dots <- as.list(substitute(list(...)))[-1]
    body_idx <- which(unlist(lapply(dots, \(x) inherits(x, "{"))))

    if (length(body_idx) != 1L) {
        stop(sprintf("Templates can define one body only. Found %s expressions.", length(body_idx)))
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
            x <- body_expr
            if (!is.null(fragment)) {
                x <- walk_nodes(x, fragment)
                !is.null(x) || stop(sprintf("Could not find fragment '%s'", fragment))
            }
            x
        },
        list(body_expr = body_expr, walk_nodes = walk_nodes)
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
    !is.null(name) || stop("A fragment cannot be defined without a name.")
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
