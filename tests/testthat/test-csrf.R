test_that("csrf works", {
    api <- plumber2::api()

    api <- plumber2::api_get(api, "/", function() {
        "foo"
    })

    api <- plumber2::api_post(api, "/bar", function() {
        "bar"
    })

    api <- api_csrf(api, secure = FALSE)

    api$trigger("freshwater")

    req <- fiery::fake_request("http://localhost:8080/", method = "get")
    res <- api$test_request(req)

    headers <- unname(unlist(res$headers, use.names = FALSE))
    csrf_header_idx <- grepl(headers, pattern = "^csrf=") |>
        which()
    csrf_header <- headers[[csrf_header_idx]]
    csrf_token <- sub("^csrf=([^;]+).*$", "\\1", csrf_header[[1]])

    expect_identical(res$status, 200L)
    expect_match(csrf_token, "^[0-9a-f]+$")
    expect_identical(nchar(csrf_token), 128L)

    # No token
    req <- fiery::fake_request("http://localhost:8080/bar", method = "post")
    res <- api$test_request(req)

    expect_identical(res$status, 403L)
    expect_identical(res$body, "[\"Invalid CSRF token\"]")

    # Correct token
    req <- fiery::fake_request(
        "http://localhost:8080/bar",
        method = "post",
        headers = list(
            `x-csrf-token` = csrf_token,
            "Cookie" = paste0("csrf=", csrf_token)
        )
    )
    res <- api$test_request(req)

    expect_identical(res$status, 200L)
    expect_identical(res$body, "[\"bar\"]")
})
