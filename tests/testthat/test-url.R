test_that("redirect works", {
    suppressMessages({
        api <- plumber2::api() |>
            plumber2::api_get("/", function(response) {
                redirect(response, "/foo")
            }) |>
            plumber2::api_get("/foo", function() {
                "Hello world!"
            })
    })

    res <- faux_request(api, path = "")

    expect_identical(res$status, 303L)
    expect_identical(res$headers$location, "/foo")
})

test_that("redirect after works", {
    suppressMessages({
        api <- plumber2::api() |>
            plumber2::api_get("/", function(response) {
                redirect(response, "/foo", after = 3L)
            }) |>
            plumber2::api_get("/foo", function() {
                "Hello world!"
            })
    })

    res <- faux_request(api, path = "")

    expect_identical(res$status, 200L)
    expect_identical(res$headers$refresh, "3; url=/foo")
})

test_that("endpoints work", {
    suppressMessages({
        api <- plumber2::api() |>
            plumber2::api_get("/foo", function(response) "foo") |>
            plumber2::api_get("/bar", function() "bar") |>
            plumber2::api_get("/baz", function() "baz") |>
            plumber2::api_delete("/baz/:id", function() "baz")
    })

    eps <- endpoints("default", api = api, refresh = TRUE)

    expect_length(eps, 3L)
    expect_identical(eps$foo(), "/foo")
    expect_identical(eps$baz$delete(id = 1L), "/baz/1")
})