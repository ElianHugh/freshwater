faux_request <- function(
    api,
    path = "",
    method = "get",
    content = "",
    accept = "text/plain; charset=utf-8",
    ...
) {
    fiery::fake_request(
        sprintf("http://localhost:8080/%s", path),
        headers = list(
            accept = accept,
            ...
        ),
        content = content,
        method = method
    ) |>
        api$test_request()
}

wait_for_resolve <- function(promise) {
    private <- attr(promise, "promise_impl")$.__enclos_env__$private
    if (is.null(private)) {
        return(promise)
    }
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

get_user_handler_from_route <- function(
    api,
    path,
    route = "default",
    method = "get"
) {
    rr <- api$request_router
    r <- rr$get_route(route)
    e <- environment(r$get_handler(method, path))
    attr(e$handler, "freshwater_hook_base", exact = TRUE)
}