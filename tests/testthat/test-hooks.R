get_user_handler_from_route <- function(api, path) {
    rr <- api$request_router
    r <- rr$get_route("default")
    e <- environment(r$get_handler("get", path))
    attr(e$handler, "freshwater_hook_base", exact = TRUE)
}


setup_dummy_api <- function(api) {
    api <- plumber2::api() |>
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
}

faux_request <- function(api, path ="", method = "get") {
    fiery::fake_request(
        sprintf("http://localhost:8080/%s", path),
        headers = list(
            accept = "text/plain; charset=utf-8"
        ),
        method = method
    ) |>
        api$test_request()
}

wait_for_resolve <- function(promise) {
    private <- attr(promise, "promise_impl")$.__enclos_env__$private
    while (private$state == "pending") {
        later::run_now(Inf, all = FALSE, loop = later::current_loop())
    }
     if (private$state == "rejected") {
        stop(conditionMessage(private$value))
    }
    if (private$visible) {
        return(private$value)
    }
    invisible(private$value)
}

intercept_hook <- hook("testthat::hook", function(api, args, next_call) {
    response <- args$response
    response$body <- "Hooked"
    return(plumber2::Break)
    # return("Hooked")
})


test_that("hooks are installed", {
    api <- setup_dummy_api() |>
        enhook_routes(hooks = intercept_hook)

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
