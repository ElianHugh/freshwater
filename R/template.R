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
#' are required. If multiple fragment names are specified, fragments will be extracted
#' and collated into a [htmltools::tagList], in the order of names provided. If a specified fragment cannot be found, an error will
#' be raised.
#'
#' # Attributes
#' Attributes with non-leading underscores are
#' rewritten as hyphenated versions instead.
#' This means you can write
#' `htmltools::div(data_foo="bar")` which is converted to
#' `htmltools::div(data-foo="bar")`.
#'
#' Attributes with trailing underscores have their underscores stripped.
#' This means the you can write `htmltools::tags$label(for_="foo")` which
#' is converted to `htmltools::tags$label(for="foo")`.
#'
#' An escape hatch exists If you explicitly want
#' underscores in your attributes.
#' You may use double underscores, which
#' will be converted to single underscores
#' e.g.
#' `htmltools::div(data__foo="bar")` which is
#' converted to `htmltools::div(data_foo="bar")`.
#'
#' # Template IDs
#'
#' Templates can define an `.id` string, or a function that resolves
#' a stable HTML `id` based on parameters passed to the template.
#' Default template arguments are provided to the id. See [target()] for
#' more information.
#'
#' # Built-in Helpers
#'
#' Template bodies are evaluated in a freshwater environment that provides
#' the following helpers:
#'
#' - `form()` — form helper with optional CSRF injection and method spoofing.
#' - `csrf_token()` — returns the current CSRF token string
#'
#' # Template Context
#' A template render context is maintained during evaluation which is
#' used for fragment extraction and cache scoping. The template context is
#' separate from the request context defined elsewhere.
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
#' # Attribute Norming
#' my_form <- template({
#'     form(
#'         label(for_ = "desc", "Description"),
#'         input(
#'             type = "text",
#'             id = "desc",
#'             data_user__id = "123"
#'         )
#'     )
#' })
#' my_form()
#'
#' @param ... template definition. Provide zero or more parameters, followed by a
#' single braced expression.
#' @param .id a character scalar or function that returns a character scalar. The result is provided as
#' an id attribute to the root of the template.
#' @param .envir the environment in which to evaluate the template
#' @return function of class `template` with interface `fn(<declared params>, ..., fragment = NULL)`
#' @rdname templating
#' @seealso [document], [cache], [form], [target], [csrf_token], [api_freshwater]
#' @export
template <- function(..., .id = NULL, .envir = rlang::caller_env()) {
    dots <- as.list(substitute(list(...)))[-1]
    body_idx <- vapply(dots, \(x) inherits(x, "{"), logical(1)) |>
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
    e$form <- .form_impl
    e$csrf_token <- .csrf_token_impl
    id <- paste0("tpl_", sprintf("%08x", sample.int(2^31 - 1L, 1L)))

    if (is.function(.id)) {
        id_fmls <- formals(.id)
        id_fml_nms <- names(id_fmls)
        tpl_nms <- names(formals_pl)
        unknown <- setdiff(id_fml_nms, tpl_nms)
        if (length(unknown)) {
            rlang::abort(
                sprintf(".id function refers to unknown template argument/s: %s", paste0(unknown, collapse = ", "))
            )
        }
        for (nm in id_fml_nms) {
            id_has_default <- !identical(id_fmls[[nm]], quote(expr = ))
            template_has_default <- !identical(formals_pl[[nm]], quote(expr = ))
            if (!id_has_default && template_has_default) {
                id_fmls[[nm]] <- formals_pl[[nm]]
            }
        }
        formals(.id) <- id_fmls
    }

    f_body <- substitute(
        {

            .fw_call_ <- rlang::caller_call()
            .fw_bottom <- rlang::current_env()
            .fw_this <- rlang::caller_env()

            withCallingHandlers(
                {
                    .fw_nm <- deparse(sys.call()[[1L]], width.cutoff = 500L)

                    .fw_runtime <- environment(sys.function())

                    assign(
                        ".freshwater_ctx",
                        list(template = .fw_nm, fragment = fragment, id = .fw_id),
                        envir = .fw_runtime
                    )
                    on.exit(rm(".freshwater_ctx", envir = .fw_runtime), add = TRUE)

                    .fw_template_body <- BODY


                    .fw_id_resolved <- NULL
                    if (!is.null(.fw_id_resolver)) {

                         if (is.character(.fw_id_resolver)) {
                            .fw_id_resolved <- .fw_id_resolver
                        } else if (is.function(.fw_id_resolver)) {
                            .fw_id_names <- names(formals(.fw_id_resolver))
                            .fw_id_args <- mget(
                                .fw_id_names,
                                envir = environment(),
                                inherits = FALSE
                            )
                            .fw_id_resolved <- do.call(.fw_id_resolver, .fw_id_args)
                        }
                        if (inherits(.fw_template_body, "shiny.tag")) {
                            .fw_template_body <- htmltools::tagAppendAttributes(
                                .fw_template_body,
                                id = .fw_id_resolved
                            )
                        }
                    }

                    if (!is.null(fragment)) {
                        .fw_template_body <- .fw_walk_nodes(.fw_template_body, fragment, .fw_nm)
                        if (is.null(.fw_template_body)) {
                            .fw_error_missing_fragment(fragment, .fw_nm)
                        }
                    }

                    .fw_rewrite_attrs(.fw_template_body, .fw_id_resolved)
                },
                error = function(e) {
                    .fw_new_template_error(
                        .fw_nm,
                        e,
                        call = .fw_call_,
                        this = .fw_this,
                        bottom = .fw_bottom
                    )
                }
            )
        },
        list(
            BODY = body_expr,
            .fw_walk_nodes = walk_nodes,
            .fw_new_template_error = new_template_error,
            .fw_rewrite_attrs = rewrite_attrs,
            .fw_error_missing_fragment = error_missing_fragment,
            .fw_id = id,
            .fw_id_resolver = .id
        )
    )

    structure(
        eval(call("function", formals_pl, f_body), envir = e),
        "template_body" = body_expr,
        "template_params" = param_exprs,
        "template_env" = .envir,
        "template_id" = id,
        "template_id_resolver" = .id,
        class = c("freshwater_template", "function")
    )
}

format_template_tree <- function(stack) {
    if (!length(stack)) {
        return(character())
    }

    els <- paste0(stack, "()")

    lapply(seq_along(els), \(i) {
        indent <- strrep(" ", i * 3L)
        branch <- if (i == length(els)) "" else "\u2514\u2500>"
        paste0(els[[i]], "\n", indent, branch)
    }) |>
        paste0(collapse = "")
}

new_template_error <- function(template_name, error, call, this, bottom) {
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

    bt <- tryCatch(
        rlang::trace_back(
            top = this,
            bottom = trace_bottom
        ),
        error = function(...) rlang::trace_back()
    )

    rlang::abort(
        message = msg,
        class = "freshwater_template_error",
        parent = cause,
        call = call,
        template_stack = stack,
        cause = cause,
        trace_bottom = trace_bottom,
        trace = bt
    )
}

rewrite_attrs <- function(tag, resolved_id) {
    if (is.null(tag)) {
        return(tag)
    }

    if (inherits(tag, "html")) {
        return(tag)
    }

    if (inherits(tag, "shiny.tag")) {
        if (length(tag$attribs)) {

            attribs <- tag$attribs
            nms <- names(attribs)


            if (".part" %in% nms) {
                val <- attribs[[".part"]]
                attribs[["data-fw-part"]] <- sprintf("%s-%s", resolved_id, val)
                attribs[[".part"]] <- NULL
            }

            nms <- names(attribs)

            # trailing -> underscores -> double underscores
            if (any(grepl("_", nms, fixed = TRUE))) {
                nms <- stringi::stri_replace_all_regex(nms, "(?<!_)_(?=$)", "")
                nms <- stringi::stri_replace_all_regex(nms, "(?<!^)(?<!_)_(?!_)", "-")
                nms <- stringi::stri_replace_all_regex(nms, "_{2,}", "_")
            }

            names(attribs) <- nms
            tag$attribs <- attribs
        }

        if (length(tag$children)) {
            tag$children <- lapply(tag$children, \(child) rewrite_attrs(child, resolved_id))
        }

        return(tag)
    }

    if (inherits(tag, "shiny.tag.list") || inherits(tag, "list")) {
        for (i in seq_along(tag)) {
            tag[i] <- list(rewrite_attrs(tag[[i]], resolved_id))
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


#' Form
#'
#' When used within a [template()], a form implementation is injected that
#' wraps `htmltools::tags$form()`.
#'
#' When a request context is available, freshwater adds optional behaviors such as
#' CSRF token insertion and HTTP method spoofing.
#'
#' Calling `form()` outside of [template()] rendering will result in an error.
#' For a plain form tag in normal R code, use [htmltools::tags]`$form()`.
#'
#' ## CSRF
#'
#' - If CSRF middleware is active, a hidden `csrf_token` input is
#' automatically injected.
#'
#' ## Method Spoofing
#' If `method` is one of "put", "patch", or "delete", a hidden `_method`
#' input is added and the HTML form method is set to "post".
#'
#' Browsers only support GET and POST.
#' When method is "put", "patch", or "delete", freshwater renders a POST form
#' with a hidden _method field. Middleware interprets this as the
#' effective HTTP method. Requires freshwater context-enabled middleware.
#'
#'
#' @param ... tag attributes and children passed to the `htmltools::tags$form()` function
#' @param method character scalar denoting the HTTP
#' method to perform.
#' One of:
#' - "get"
#' - "post"
#' - "put"
#' - "patch"
#' - "delete"
#' @return (When injected) An `htmltools::tag` object.
#' @seealso [template], [api_csrf], [api_freshwater], [htmltools::tags]
#' @examples
#' page <- template({
#'      form(method = "delete")
#' })
#' page()
#' @export
form <- function(..., method = "get") {
    rlang::abort(
        c(
            "freshwater::form is a stub and cannot be called directly.",
            i = "Use form() inside `template()` rendering",
            i = "For a plain form tag in normal R code, consider `htmltools::tags$form()`."
        ),
        class = "freshwater_builtin_stub"
    )
}

#' HTML Document Root
#' @description
#' Constructs a full HTML document. It does not modify or validate its contents.
#'
#' `document()` is used for full-page responses, and should not be used for partials, fragments
#' or nested templates.
#' @return An `htmltools::tagList`, consisting of a doctype declaration, an <html> tag, and
#' user-supplied content.
#' @param ... user-supplied content
#' @examples
#' document(
#'      htmltools::tags$head(
#'          htmltools::tags$title("Home")
#'      ),
#'      htmltools::tags$body(
#'          htmltools::tags$h1("Hello")
#'      )
#' )
#' @seealso [template], [fragment]
#' @export
document <- function(...) {
    htmltools::tagList(
        htmltools::HTML("<!DOCTYPE html>"),
        htmltools::tags$html(...)
    )
}

.form_impl <- function(..., method = "get") {
    ctx <- get_fw_context()
    children <- list(...)

    method <- tolower(method)
    method <- match.arg(method, c("get", "post", "put", "patch", "delete"))

    if (method %in% c("put", "patch", "delete")) {
        faux_method <- htmltools::tags$input(
            type = "hidden",
            name = "_method",
            value = toupper(method)
        )
        children <- c(list(faux_method), children)
        method <- "post"
    }

    if (!is.null(ctx)) {
        if (!is.null(ctx$csrf_token) && method != "get") {
            token_input <- htmltools::tags$input(
                type = "hidden",
                name = "csrf_token",
                value = .csrf_token_impl()
            )
            children <- c(list(token_input), children)
        }
    }

    do.call(
        htmltools::tags$form,
        c(list(method = method), children)
    )
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

walk_one <- function(tag, name, tpl_nm) {
    out <- vector(mode = "list", length = 0L)
    walk <- function(x) {
        if (inherits(x, "shiny.tag")) {
            fragment <- attr(x, "fragment")
            if (!is.null(fragment) && identical(fragment, name)) {
                out[[length(out) + 1L]] <<- x
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


    if (length(out) == 0L) {
        error_missing_fragment(name, tpl_nm)
    }

    if (length(out) == 1L) {
        return(out[[1L]])
    }

    htmltools::tagList(out)


}

walk_nodes <- function(tag, name, tpl_nm) {
    names_requested <- unique(as.character(name))

    if (length(names_requested) == 1L) {
        return(walk_one(tag, names_requested[[1L]], tpl_nm))
    }

    lookup <- rep.int(TRUE, length(names_requested))
    names(lookup) <- names_requested
    buckets <- structure(
        vector(
            mode = "list",
            length = length(names_requested)
        ),
        names = names_requested
    )
    walk <- function(x) {
        if (inherits(x, "shiny.tag")) {
            fragment <- attr(x, "fragment")
            if (!is.null(fragment) && isTRUE(lookup[fragment])) {
                buckets[[fragment]][[length(buckets[[fragment]]) + 1L]] <<- x
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

    lens <- lengths(buckets)
    if (any(lens == 0L)) {
        idx <- which(lens==0L)
        nms <- names(buckets)[idx]
        error_missing_fragment(nms, tpl_nm)
    }

    found <- unlist(buckets, recursive = FALSE, use.names = FALSE)

    if (!length(found)) {
        return(NULL)
    }

    if (length(found) == 1L) {
        return(found[[1L]])
    }

    htmltools::tagList(found)
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


#' Resolve a template's target selector
#'
#' `target()` returns a CSS selector for a template instance. For normal
#' targets, this is an id selector of the form `#<TEMPLATE_ID>`, where
#' the id is resolved from the template's id function.
#'
#' In order to support multi-root template selection,
#' supplying `.part` will return a data attribute selector of the form:
#' `[data-fw-part="<TEMPLATE_ID>-<PART_NAME>"]`
#' Part names are automatically scoped against the template's `.id`, ensuring
#' unique data attributes across templates.
#'
#' @param tpl a freshwater template
#' @param ... arguments passed to the template id function
#' @param .part whether to select a sub-part
#' @examples
#' card <- template(
#'     user,
#'     .id = function(user) sprintf("user-%s", user$id),
#'     {
#'         div(user$name)
#'     }
#' )
#' target(card, list(id = 1234L))
#'
#' user_table <- template(users = list(), .id = "my-table", {
#'     table(
#'         thead(
#'             .part = "header",
#'             tr(th("Name"))
#'         ),
#'         tbody(
#'             .part = "body",
#'             lapply(users, \(user) tr(td(user)))
#'         ),
#'         tfoot(
#'             .part = "footer",
#'             tr(td(sprintf("There are '%s' users.", length(users))))
#'         )
#'    )
#' })
#' target(user_table, .part = "body")
#' @seealso [targets], [template]
#' @export
target <- function(tpl, ..., .part = NULL) {
    tpl_id <- attr(tpl, "template_id_resolver", exact = TRUE)
    if (is.null(tpl_id)) {
        fw_nm <- deparse(substitute(tpl), width.cutoff = 500L)
        msg <- sprintf("No instance # defined for template `%s`.", fw_nm)
        rlang::abort(msg, class = "freshwater_template_error")
    }
    if (is.character(tpl_id)) {
        res <- tpl_id
    } else if (is.function(tpl_id)) {
        res <- tryCatch(
            tpl_id(...),
            error = function(e) {
                fw_nm <- deparse(substitute(tpl), width.cutoff = 500L)
                rlang::abort(
                    sprintf(
                        "Failed to resolve target for template `%s`: %s",
                        fw_nm,
                        e$message
                    ),
                    class = "freshwater_template_error"
                )
            }
        )
    } else {
        rlang::abort(
            sprintf(
                "Unexpected value for template ID. Expected `character` or `function`, received `%s`",
                class(tpl_id)
            )
        )
    }

    if (!is.null(.part)) {
        return(sprintf('[data-fw-part="%s-%s"]', res, .part))
    }

    sprintf("#%s", res)
}

#' Combine multiple target selectors
#'
#' `targets()` returns a comma-separated string combining multiple
#' [target()] calls. If a template is supplied, `target()` is called on it,
#' otherwise the value is coerced to character and used as-is.
#'
#' @param ... templates or character vectors
#' @examples
#' tpl <- template(.id = "foo", {})
#' tpl2 <- template(x, .id = function(x) x, {})
#' targets(
#'     tpl,
#'    target(tpl2, x = "bar")
#' )
#' @seealso [target]
#' @export
targets <- function(...) {
    dots <- Filter(Negate(is.null), list(...))
    res <- lapply(dots, \(x) {
        if (is.function(x)) {
            target(x)
        } else {
            as.character(x)
        }
    })
    paste0(unlist(res, use.names = FALSE), collapse = ", ")
}

#' Apply template function to each element of a vector
#'
#' @description
#' `map_tags()` returns a type-safe tag list,
#' where each element is resolved by applying `.f` to each
#' element of `.x`. If an element in `.x` is a falsey value
#' (i.e. `NA`, `NaN`, `FALSE`, or `NULL`), the fallback value
#' from `.empty` is used.
#'
#' Additional arguments should be passed
#' with an anonymous function.
#'
#' @details
#' Element values are evaluated prior to returning the tag list:
#' - NULL values are removed from the final tag list.
#'  The return length of `map_tags()` is therefore less
#'  than or equal to the length of `.x`
#' - All return values must be either a "shiny.tag",
#'   "shiny.tag.list", or "character" vector. An
#'   error is raised if an unexpected value is encountered.
#' @param .x list or atomic vector
#' @param .f a function that takes a single argument returns a character
#' vector, tag, or tagList.
#' @param .empty fallback value for falsey elements
#' @seealso [template], [base::lapply], [htmltools::tagList]
#' @examples
#' tpl <- template(x, {p(x)})
#' map_tags(seq(5L), tpl)
#'
#' # falsey values are removed
#' map_tags(c(TRUE, FALSE, TRUE), tpl)
#' @export
map_tags <- function(.x, .f, .empty = NULL) {
    res <- lapply(.x, function(x) {
        if (is.null(x) || isFALSE(x) || is.na(x)) return(.empty)
        .f(x)
    })

    hits <- vapply(
        res,
        function(x) {
            inherits(x, c("shiny.tag.list", "shiny.tag")) ||
                is.character(x) ||
                is.null(x)
        },
        logical(1L),
        USE.NAMES = FALSE
    )

    if (!all(hits)) {
        bad <- which(!hits)
        bad_cls <- vapply(
            res[bad],
            function(x) paste(class(x), collapse = "/"),
            character(1L)
        ) |>
            unlist(use.names = FALSE)

        rlang::abort(
            c(
                "Bad value returned from `.f`.",
                i = "Expected either `NULL`, `character`, `shiny.tag` or `shiny.tag.list`.",
                x = sprintf("Bad indices: %s", paste0(bad, collapse = ", ")),
                x = sprintf("Bad classes: %s", paste0(bad_cls, collapse = ", "))
            ),
            class = "freshwater_template_error"
        )
    }

    res <- Filter(Negate(is.null), res)

    htmltools::as.tags(res)
}