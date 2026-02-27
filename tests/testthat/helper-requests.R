faux_request <- function(
    api,
    path = "",
    method = "get",
    accept = "text/plain; charset=utf-8"
) {
    fiery::fake_request(
        sprintf("http://localhost:8080/%s", path),
        headers = list(
            accept = accept
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