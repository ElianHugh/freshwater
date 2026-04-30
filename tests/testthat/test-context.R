
setup_dummy_api <- function(api) {
    suppressMessages(expr = {
        plumber2::api() |>
            plumber2::api_get("/", function() {
                "OK!"
            }) |>
            plumber2::api_get("/fail", function() {
                stop("Failure")
            }) |>
            plumber2::api_get(
                "/async",
                function() {
                    123L
                },
                async = TRUE
            ) |>
            plumber2::api_get(
                "/async_then",
                function() {
                    123L
                },
                then = list(
                    function(response) {
                        response$body <- 5L
                        plumber2::Next
                    }
                ),
                async = TRUE
            )
    })
}

test_that("context is installed", {
    api <- setup_dummy_api()
    api <- api_context(api)
    api$trigger("freshwater::hook")

    user_fn <- get_user_handler_from_route(api, "/")
    hooks <- attr(user_fn, "freshwater_hooks", exact = TRUE)
    expect_type(user_fn, "closure")
    expect_length(hooks, 1L)
    expect_identical(attr(hooks[[1]], "freshwater_hook_id"), "freshwater::context")
})

test_that("context is available", {
    res <- FALSE
    api <- suppressMessages(
        plumber2::api() |>
            plumber2::api_get("/foo", function(request) {
                ctx <- get_fw_context()
                res <<- identical(ctx$request, request)
            })
    )
    api <- api_context(api)
    api$trigger("freshwater::hook")
    faux_request(api, path = "foo", accept = "text/html; charset=utf-8")
    expect_true(res)
})

# test_that("context is available when constructing a promise", {
#     ok <- FALSE
#     api <- suppressMessages(
#         plumber2::api() |>
#             plumber2::api_get("/foo", function(request) {

#                 promises::promise(function(resolve, reject) {
#                     ctx <- get_fw_context()
#                     ok <<- !is.null(ctx) && identical(ctx$request, request)

#                     # note, does NOT work inside the later/async part
#                     later::later(function() { resolve("done") }, 0)
#                 })
#             })
#     )
#     api <- api_context(api)
#     api$trigger("freshwater::hook")
#     res <- faux_request(api, path = "foo", accept = "text/html; charset=utf-8") |>
#         wait_for_resolve()
#     expect_true(ok)
# })


test_that("current path works", {
    api <- suppressMessages(
        plumber2::api() |>
        plumber2::api_get("/foo", function() {
            current_path()
        })
    )

    api <- api_context(api)
    api$trigger("freshwater::hook")

    res <- faux_request(api, path = "foo", accept = "text/html; charset=utf-8")
    expect_identical(res$body, "/foo")

    expect_error(
        current_path(),
        class = "freshwater_context_missing"
    )
})

test_that("current_* helpers work", {
    tpl <- template(
        cookie = NULL,
        header = NULL,
        query = NULL,
        path = NULL,
        method = NULL,
        {
            div(
                p(cookie %||% current_cookie("theme")),
                p(header %||% current_header("accept")),
                p(query %||% current_query()$id),
                p(path %||% current_path()),
                p(method %||% current_method())
            )
        }
    )

    api <- suppressMessages(
        plumber2::api() |>
            plumber2::api_get("/foo", function() {
                tpl()
            })
    )

    api <- api_freshwater(api, csrf = FALSE, error_pages = FALSE)
    api$trigger("freshwater::hook")

    res <- faux_request(
        api,
        path = "foo?id=1",
        accept = "text/html; charset=utf-8",
        cookie = "theme=dark"
    )
    expect_identical(
        res$body,
        tpl(
            cookie = "dark",
            header = "text/html; charset=utf-8",
            query = 1L,
            path = "/foo",
            method = "get"
        ) |> htmltools::doRenderTags() |>
            as.character()
    )
})


test_that("current_* helpers work under fw async", {
    testthat::skip_if_not_installed("mori")
    testthat::skip_if_not_installed("mirai")
    testthat::skip_if_not_installed("promises")

    register_async_evaluator()
    tpl <- template(
        cookie = NULL,
        header = NULL,
        query = NULL,
        path = NULL,
        method = NULL,
        {
            div(
                p(cookie %||% current_cookie("theme")),
                p(header %||% current_header("accept")),
                p(query %||% current_query()$id),
                p(path %||% current_path()),
                p(method %||% current_method())
            )
        }
    )

    api <- suppressMessages(
        plumber2::api() |>
            plumber2::api_get("/foo", function() {
                tpl()
            }, async = "freshwater")
    )

    api <- api_freshwater(api, csrf = FALSE, error_pages = FALSE)
    api$trigger("freshwater::hook")

    res <- faux_request(
        api,
        path = "foo?id=1",
        accept = "text/html; charset=utf-8",
        cookie = "theme=dark"
    ) |>
        wait_for_resolve()


    expect_identical(
        res$body,
        tpl(
            cookie = "dark",
            header = "text/html; charset=utf-8",
            query = 1L,
            path = "/foo",
            method = "get"
        ) |>
            htmltools::doRenderTags() |>
            as.character()
    )
})
