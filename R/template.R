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
#' # Attributes
#' Attributes with non-leading underscores are
#' rewritten as hyphenated versions instead.
#' This means you can write
#' `htmltools::div(data_foo="bar")` which is converted to
#' `htmltools::div(data-foo="bar")`.
#'
#' An escape hatch exists If you explicitly want
#' underscores in your attributes.
#' You may use double underscores, which
#' will be converted to single underscores
#' e.g.
#' `htmltools::div(data__foo="bar")` which is
#' converted to `htmltools::div(data_foo="bar")`.
#'
#' @examples
#' # Example Fragment Usage
#' page_main <- template(
#'     {
#'         div(
#'             h1("Dashboard"),
#'             fragment(
#'                  p("Welcome back"),
#'                  name = "content"
#'             ),
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
#'     ttl, footer = NULL, {
#'         div(
#'             h2(ttl),
#'             fragment(div("Card body"), name="body"),
#'             if (!is.null(footer)) {
#'                 fragment(
#'                     div(footer),
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
#' @seealso [cache]
#' @export
template <- function(..., .envir = parent.frame()) {
    dots <- as.list(substitute(list(...)))[-1]
    body_idx <- lapply(dots, \(x) inherits(x, "{") )|>
            unlist() |>
            which()

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

    formals_pl <- as.pairlist(c(
        param_exprs,
        alist(... = ),
        list(fragment = NULL)
    ))

    e <- new.env(parent = .envir)
    list2env(as.list(htmltools::tags), envir = e)
    e$form <- form
    e$csrf_token <- csrf_token
    id <- paste0("tpl_", sprintf("%08x", sample.int(2^31 - 1L, 1L)))

    f_body <- substitute(
        {
            withCallingHandlers(
                {
                    nm <- deparse(sys.call()[[1L]], width.cutoff = 500L)

                    assign(
                        ".freshwater_ctx",
                        list(template = nm, fragment = fragment, id = id),
                        envir = env
                    )
                    on.exit(rm(".freshwater_ctx", envir = env), add = TRUE)


                    x <- local({body_expr})

                    if (!is.null(fragment)) {
                        x <- walk_nodes(x, fragment)
                        if (is.null(x)) {
                            error_missing_fragment(fragment, nm)
                        }
                    }

                    rewrite_attrs(x)
                },
                error = function(e) {
                    call_ <- sys.call()
                    bottom <- rlang::current_env()
                    new_template_error(nm, e, call = call_, bottom = bottom)
                }
            )
        },
        list(
            body_expr = body_expr,
            walk_nodes = walk_nodes,
            new_template_error = new_template_error,
            rewrite_attrs = rewrite_attrs,
            error_missing_fragment = error_missing_fragment,
            env = e,
            id = id
        )
    )

    structure(
        eval(call("function", formals_pl, f_body), e),
        "template_body" = body_expr,
        "template_params" = param_exprs,
        "template_env" = .envir,
        "template_id" = id,
        "template_name" = "name",
        class = c("freshwater_template", "function")
    )
}


rewrite_attrs <- function(tag) {
    if (inherits(tag, "html")) {
        return(tag)
    }

    if (inherits(tag, "shiny.tag.list")) {

        for (i in seq_along(tag)) {
            tag[[i]] <- rewrite_attrs(tag[[i]])
        }
        return(tag)
    }

    if (inherits(tag, "shiny.tag")) {

        if (length(tag$attribs)) {
            attribs <- tag$attribs
            nms <- names(attribs)

            nms <- gsub("(?<!^)(?<!_)_(?!_)", "-", nms, perl = TRUE)
            nms <- gsub("_{2,}", "_", nms, perl = TRUE)

            names(attribs) <- nms
            tag$attribs <- attribs
        }

        if (length(tag$children)) {
            tag$children <- lapply(tag$children, rewrite_attrs)
        }

        return(tag)
    }

    tag
}

#' @rdname templating
#' @param name the name of the fragment
#' @export
fragment <- function(name = NULL, ...) {
    !is.null(name) || error_fragment_definition()

    x <- htmltools::as.tags(...)

    if (inherits(x, "shiny.tag.list")) {
        for (i in seq_along(x)) {
            if (inherits(x[[i]], "shiny.tag")) {
                attr(x[[i]], "fragment") <- name
            } else if (inherits(x[[i]], "list")) {
                for (j in seq_along(x[[i]])) {
                    attr(x[[i]][[j]], "fragment") <- name
                }
            }
        }
    } else {
        attr(x, "fragment") <- name
    }

    class(x) <- unique(c("freshwater_fragment", class(x)))

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
                params_string <- c(
                    params_string,
                    paste0(nm, " = ", deparse(param))
                )
            }
        }
    }

    out <- "template(%s)\n%s\n%s"

    out <- sprintf(
        out,
        paste0(
            c(params_string, "...", "fragment = NULL"),
            collapse = ", "
        ),
        paste0(deparse(body), collapse = "\n"),
        format(e)
    )

    cat(out)
}

#' @exportS3Method
print.freshwater_fragment <- function(x, ...) {
    if (interactive()) {
        cat("[fragment]\n")
    }
    NextMethod()
}

walk_nodes <- function(tag, name) {
    found <- list()
    walk <- function(x) {

        if (inherits(x, "shiny.tag")) {

            fragment <- attr(x, "fragment")

            if (!is.null(fragment) && identical(fragment, name)) {
                found[[length(found) + 1L]] <<- x
            }

            for (i in seq_along(x$children)) {
                walk(x$children[[i]])
            }
            return(invisible(NULL))

        } else if (inherits(x, "list")) {
            for (child in x) {
                walk(child)
            }
            return(invisible(NULL))
        }
        invisible(NULL)
    }

    walk(tag)


    if (!length(found)) {
        return(NULL)
    }

    if (length(found) == 1L) {
        return(found[[1L]])
    }

    htmltools::tagList(found)
}

format_template_tree <- function(stack) {
    if (!length(stack)) return(character())

    els <- paste0(stack, "()")

    lapply(seq_along(els), \(i) {
        indent <- strrep(" ", i * 3L)
        branch <- if (i == length(els)) "" else "\u2514\u2500>"
        paste0(els[[i]], "\n", indent, branch)
    }) |>
        paste0(collapse = "")
}

new_template_error <- function(template_name, error, call, bottom) {
    stack <- c(template_name, error$template_stack)

    cause <- error$cause %||% error
    trace_bottom <- error$trace_bottom %||% bottom

    if (length(stack) <= 1L) {
        msg <- "Error while rendering template:"
    } else {
        tree <- format_template_tree(stack)
        msg <- c(
            "Error while rendering template(s):",
            tree
        )
    }

    rlang::abort(
        message = msg,
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

current_template <- function(
    env = parent.frame(),
    default = list(template = "anonymous_template", fragment = NULL, id = NULL)
) {
    ctx <- get0(".freshwater_ctx", envir = env, inherits = TRUE)
    if (is.null(ctx)) default else ctx
}
