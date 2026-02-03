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
#'  passes `...`  to the containing HTML nodes
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
        error_template_single_body(body_idx)
    }

    body_expr <- dots[body_idx][[1L]]

    param_exprs <- dots[-body_idx]
    param_names <- names(param_exprs) %||% character(length(param_exprs))

    for (i in seq_along(param_exprs)) {
        if (identical(param_names[i], "") || is.null(param_names[i])) {
            if (!is.symbol(param_exprs[[i]])) {
                error_template_bare_symbols(deparse(param_exprs[[i]]))
            }

            param_names[[i]] <- as.character(param_exprs[[i]])
            param_exprs[[i]] <- quote(expr = )
        }
    }

    names(param_exprs) <- param_names

    if ("fragment" %in% names(param_exprs)) {
        error_reserved_argument()
    }

    formals_pl <- as.pairlist(c(param_exprs, alist(...=), list(fragment = NULL)))


    e <- new.env(parent = .envir)
    list2env(as.list(htmltools::tags), envir = e)

    f_body <- substitute(
        {
            withCallingHandlers(
                {
                    call_ <- sys.call()
                    bottom <- rlang::current_env()
                    nm <- deparse(sys.call()[[1L]], width.cutoff = 500L)

                    x <- body_expr

                    if (!is.null(fragment)) {
                        x <- walk_nodes(x, fragment)
                        if (is.null(x)) {
                            error_missing_fragment(fragment, nm)
                        }
                    }

                    x

                },
                error = function(e) {
                    new_template_error(nm, e, call = call_, bottom = bottom)
                }
            )
        },
        list(
            body_expr = body_expr,
            walk_nodes = walk_nodes,
            env = e
        )
    )

    structure(
        eval(call("function", formals_pl, f_body), e),
        "template_body" = body_expr,
        "template_params" = param_exprs,
        "template_env" = .envir,
        class = c("freshwater_template", "function")
    )
}

#' @rdname templating
#' @param name the name of the fragment
#' @export
fragment <- function(..., name = NULL) {
    !is.null(name) || error_fragment_definition()
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

format_template_tree <- function(stack) {
    if (!length(stack)) return(character())

    els <- paste0(stack, "()")

    lapply(seq_along(els), \(i) {
        el <- els[[i]]

        el_last <- if (i > 1) {
            el_last <- els[[i - 1]]
        } else {
            ""
        }

        indent <- strrep(" ", i * 3L)
        branch <- if (i == length(els)) "" else "└─>"
        paste0(el, "\n", indent, branch)
    }) |>
        paste0(collapse="")
}

new_template_error <- function(template_name, error, call, bottom) {
    stack <- c(template_name, error$template_stack)
    tree <- format_template_tree(stack)
    cause <- error$cause %||% error
    trace_bottom <- error$trace_bottom %||% bottom

    rlang::abort(
        message = c(
            "Error while rendering template(s):",
            tree
        ),
        class = "freshwater_template_error",
        parent = cause,
        call = call,
        template_stack = stack,
        cause = cause,
        trace_bottom = trace_bottom,
        .frame = bottom,
        .trace_bottom = trace_bottom
    )
}

error_template_single_body <- function(indices, call = rlang::caller_env()) {
    msg <- sprintf(
        "Templates can define one body only. Found %s expressions.",
        length(indices)
    )
    rlang::abort(msg, call = call)
}

error_template_bare_symbols <- function(sym, call = rlang::caller_env()) {
    msg <- sprintf(
        "Unnamed parameters must be bare symbols: %s",
        sym
    )
    rlang::abort(msg, call = call)
}

error_reserved_argument <- function(call = rlang::caller_env()) {
    rlang::abort(
        "Duplicate `fragment` parameter found. `fragment` is a reserved template argument.",
        call = call
    )
}

error_missing_fragment <- function(fragment, nm, call = rlang::caller_env()) {
    msg <- sprintf(
        "Could not find fragment '%s' in template `%s`",
        fragment,
        nm
    )
    rlang::abort(
        msg,
        class = "freshwater_template_error",
        call = call
    )
}

error_fragment_definition <- function(call = rlang::caller_env()) {
    rlang::abort(
        "A fragment cannot be defined without a name.",
        call = call
    )
}
