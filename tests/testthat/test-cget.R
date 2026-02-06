testthat::skip_if_not_installed("fiery")

test_that("304 is returned on matching etag", {
    x <- 1L
    pa <- plumber2::api() |>
        plumber2::api_get("/", function() {
            x
        }) |>
        api_cget("/", \() x)

    req <- fiery::fake_request(
        "http://localhost:8080/",
        headers = list(
            `If-None-Match` = "W/\"1\""
        )
    )

    res <- pa$test_request(req)

    expect_identical(res$status, 304L)
})


