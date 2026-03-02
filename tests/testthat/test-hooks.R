get_user_handler_from_route <- function(api, path) {
    rr <- api$request_router
    r <- rr$get_route("default")
    e <- environment(r$get_handler("get", path))
    attr(e$handler, "freshwater_hook_base", exact = TRUE)
}

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

intercept_hook <- hook("testthat::hook", function(api, args, next_call) {
    response <- args$response
    response$body <- "Hooked"
    plumber2::Break
})

pass_hook <- hook(
    id = "testthat::pass",
    function(api, args, next_call) next_call()
)

test_that("plumber handler env is patchable", {
    api <- setup_dummy_api()

    rr <- api$request_router
    r <- rr$get_route("default")

    # Sync handler
    e <- environment(r$get_handler("get", "/"))
    expect_true("handler" %in% names(e))
    expect_true(is.function(e$handler))

    # Async handler
    e <- environment(r$get_handler("get", "/async_then"))
    expect_true("async" %in% names(e))
    expect_true("then" %in% names(e))
    expect_true(is.list(e$then) || inherits(e$then, "list"))
})

test_that("hooks are installed", {
    api <- setup_dummy_api() |>
        enhook_routes(hooks = list(intercept_hook))

    user_fn <- get_user_handler_from_route(api, "/")
    hooks <- attr(user_fn, "freshwater_hooks", exact = TRUE)
    expect_type(user_fn, "closure")
    expect_length(hooks, 1L)

    res <- faux_request(api, path = "")
    expect_identical(res$status, 200L)
    expect_identical(res$body, "Hooked")

    # should also intercept the fail route
    res <- faux_request(api, path = "fail")
    expect_identical(res$status, 200L)
    expect_identical(res$body, "Hooked")

    # should NOT hook the async route
    res <- wait_for_resolve(faux_request(api, path = "async"))
    expect_identical(res$status, 200L)
    expect_identical(res$body, "123")

    # SHOULD hook the then of an async route
    res <- wait_for_resolve(faux_request(api, path = "async_then"))
    expect_identical(res$status, 200L)
    expect_identical(res$body, "Hooked")
})

test_that("allow errors when passing", {
    api <- setup_dummy_api() |>
        enhook_routes(hooks = list(pass_hook))
    res <- faux_request(api, path = "fail")
    expect_identical(res$status, 500L)
    expect_identical(res$body, "Internal Server Error")
})
