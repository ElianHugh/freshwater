testthat::skip_if_not_installed("openssl")

test_that("csrf works", {

    suppressMessages({
        api <- plumber2::api()

        api <- plumber2::api_get(api, "/", function() {
            "foo"
        })
        api <- plumber2::api_post(api, "/bar", function() {
            "bar"
        })
        api <- plumber2::api_delete(api, "/bar", function() {
            "bar"
        })
        api <- api_csrf(api, secure = FALSE)
        api <- plumber2::api_security_headers(api)
        api <- plumber2::api_security_cors(api)
        api <- plumber2::api_security_resource_isolation(api)
    })




    api$trigger("freshwater::hook")

    req <- fiery::fake_request("https://localhost:8080/", method = "get")
    res <- api$test_request(req)

    headers <- unname(unlist(res$headers, use.names = FALSE))
    csrf_header_idx <- grepl(headers, pattern = "^csrf=") |>
        which()
    csrf_header <- headers[[csrf_header_idx]]
    csrf_token <- sub("^csrf=([^;]+).*$", "\\1", csrf_header[[1]])

    expect_identical(res$status, 200L)
    expect_match(csrf_token, "^[0-9a-f]+$")
    expect_identical(nchar(csrf_token), 128L)

    # No token (POST)
    for (method in c("post", "delete")) {
        req <- fiery::fake_request(
            "https://localhost:8080/bar",
            method = method
        )
        res <- api$test_request(req)

        expect_identical(res$status, 403L)
        expect_identical(res$body, "[\"Invalid CSRF token\"]")
    }

    # Correct token
    for (method in c("post", "delete")) {
        req <- fiery::fake_request(
            "https://localhost:8080/bar",
            method = method,
            headers = list(
                `x-csrf-token` = csrf_token,
                "Cookie" = paste0("csrf=", csrf_token)
            )
        )
        res <- api$test_request(req)

        expect_identical(res$status, 200L)
        expect_identical(res$body, "[\"bar\"]")
    }
})


test_that("404s still occur with CSRF on", {
    api <- plumber2::api()
    api <- api_csrf(api, secure = FALSE)
    api$trigger("freshwater::hook")

    req <- fiery::fake_request("https://localhost:8080/", method = "get")
    res <- api$test_request(req)

    expect_identical(res$status, 404L)
})

test_that("constant time comparison doesn't leak", {
    flip_first <- function(s) {
        substr(s, 1, 1) <- if (substr(s, 1, 1) == "A") "B" else "A"
        s
    }
    flip_last <- function(s) {
        n <- nchar(s)
        substr(s, n, n) <- if (substr(s, n, n) == "A") "B" else "A"
        s
    }
    time_batch <- function(fun, a, b, reps = 5000L) {
        gc()
        t0 <- proc.time()[["elapsed"]]
        for (i in seq_len(reps)) {
            fun(a, b)
        }
        proc.time()[["elapsed"]] - t0
    }

    token1 <- csrf_new_token()
    token2 <- csrf_new_token()
    token1_late <- flip_last(token1)
    token1_early <- flip_first(token1)

    expect_true(constant_time_identical(token1, token1))
    expect_false(constant_time_identical(token1, token2))
    expect_false(constant_time_identical(token1, token1_late))
    expect_false(constant_time_identical(token1, token1_early))

    skip_if_not(nzchar(Sys.getenv("FW_TIMING_TESTS")))

    times <- c(
        time_batch(constant_time_identical, token1, token1),
        time_batch(constant_time_identical, token1, token2),
        time_batch(constant_time_identical, token1, token1_late),
        time_batch(constant_time_identical, token1, token1_early)
    )
    expect_lt(max(times) / min(times), 1.25)
})

test_that("csrf token is available during first page render", {
    suppressMessages({
        api <- plumber2::api()

        page <- template({
            html(
                head(
                    meta(name = "csrf-token", content = csrf_token())
                ),
                body(
                    form(
                        action = "/bar",
                        method = "post",
                        input(type = "submit", value = "OK")
                    )
                )
            )
        })

        api <- plumber2::api_get(api, "/", function() {
            page()
        })

        api <- plumber2::api_post(api, "/bar", function() {
            "bar"
        })

        api <- api_csrf(api, secure = FALSE)
    })

    api$trigger("freshwater::hook")

    req <- fiery::fake_request(
        "https://localhost:8080/",
        method = "get",
        headers = list(
            accept = "text/html; charset=utf-8"
        )
    )
    res <- api$test_request(req)

    expect_identical(res$status, 200L)

    headers <- unname(unlist(res$headers, use.names = FALSE))
    csrf_header_idx <- grepl("^csrf=", headers) |> which()
    expect_true(length(csrf_header_idx) >= 1)

    csrf_header <- headers[[csrf_header_idx[[1]]]]
    csrf_token <- sub("^csrf=([^;]+).*$", "\\1", csrf_header)

    expect_match(res$body, csrf_token)
})

test_that("csrf works with async", {
    testthat::skip_if_not_installed("mori")
    testthat::skip_if_not_installed("mirai")
    testthat::skip_if_not_installed("promises")

    register_async_evaluator()
    register_html_serialiser()

    tpl <- template({
        htmltools::tags$html(
            head(
                meta(name = "csrf-token", content = csrf_token())
            ),
            body(
                form(
                    action = "/bar",
                    method = "post",
                    input(type = "submit", value = "OK")
                )
            )
        )
    })

    api <- suppressMessages(
        plumber2::api() |>
        api_csrf(secure = FALSE) |>
            plumber2::api_get(
                "/foo",
                function() {
                    tpl()
                },
                async = "freshwater"
            ) |>
           plumber2::api_post("/bar", function() "bar")
    )

    api$trigger("freshwater::hook")

    res <- faux_request(
        api,
        "foo",
        method = "get",
        accept = "text/html; charset=utf-8"
    ) |>
        wait_for_resolve()

    expect_identical(res$status, 200L)
    headers <- unname(unlist(res$headers, use.names = FALSE))
    csrf_header_idx <- grepl("^csrf=", headers) |> which()
    expect_true(length(csrf_header_idx) >= 1)

    csrf_header <- headers[[csrf_header_idx[[1]]]]
    csrf_token <- sub("^csrf=([^;]+).*$", "\\1", csrf_header)
    expect_match(res$body, csrf_token)
})


test_that("csrf rejects unsafe async", {
    # for some reason, these tests always fail when run interactively
    testthat::skip_if_not_installed("mori")
    testthat::skip_if_not_installed("mirai")
    testthat::skip_if_not_installed("promises")

    register_async_evaluator()
    register_html_serialiser()

    api <- suppressMessages(
        plumber2::api() |>
            api_csrf(secure = FALSE) |>
            plumber2::api_post(
                "/bar",
                function() {
                    "ok"
                },
                async = "freshwater"
            )
    )
    api$trigger("freshwater::hook")

    res <- faux_request(api, "bar", method = "post") |>
        wait_for_resolve()

    expect_identical(res$status, 403L)

    token <- "abc123"
    res <- faux_request(
        api,
        "bar",
        method = "post",
        cookie = paste0("csrf=", token),
        "x-csrf-token" = token
    ) |>
        wait_for_resolve()
     expect_identical(res$status, 200L)


    res <- faux_request(
        api,
        "bar",
        method = "post",
        cookie = "csrf=real-token",
        "x-csrf-token" = "wrong-token"
    ) |>
        wait_for_resolve()

    expect_identical(res$status, 403L)
})
