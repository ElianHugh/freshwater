testthat::skip_if_not_installed("openssl")

test_that("csrf works", {
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

    api$trigger("freshwater_csrf")

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
    api$trigger("freshwater_csrf")

    req <- fiery::fake_request("https://localhost:8080/", method = "get")
    res <- api$test_request(req)

    expect_identical(res$status, 404L)
})
