test_that("redirect works", {
    suppressMessages({
        api <- plumber2::api() |>
            plumber2::api_get("/", function(response) {
                redirect(response, "/foo")
            }) |>
            plumber2::api_get("/foo", function() {
                "Hello world!"
            }) |>
            plumber2::api_get("/bad", function(response) {
                redirect(response, "http://www.google.com")
            })
    })

    res <- faux_request(api, path = "")

    expect_identical(res$status, 303L)
    expect_identical(res$headers$location, "/foo")


    # redirect should fail for cross-origin URLs
    res <- faux_request(api, path = "bad")
    expect_identical(res$status, 500L)
})

test_that("redirect after works", {
    suppressMessages({
        api <- plumber2::api() |>
            plumber2::api_get("/", function(response) {
                redirect(response, "/foo", after = 3L)
            }) |>
            plumber2::api_get("/foo", function() {
                "Hello world!"
            }) |>
            plumber2::api_get("/bad", function(response) {
                redirect(response, "http://www.google.com", after = 3L)
            })
    })

    res <- faux_request(api, path = "")

    expect_identical(res$status, 200L)
    expect_identical(res$headers$refresh, "3; url=/foo")


    res <- faux_request(api, path = "bad")
    expect_identical(res$status, 500L)
    expect_identical(res$headers$refresh, NULL)

})

test_that("endpoints work", {
    suppressMessages({
        api <- plumber2::api() |>
            plumber2::api_get("/foo", function(response) "foo") |>
            plumber2::api_get("/bar", function() "bar") |>
            plumber2::api_get("/baz", function() "baz") |>
            plumber2::api_get("/baz/:name", function() "baz") |>
            plumber2::api_delete("/baz/:id", function() "baz")
    })

    eps <- endpoints("default", api = api, refresh = TRUE)

    expect_length(eps, 3L)
    expect_identical(eps$foo(), "/foo")
    expect_identical(eps$baz$delete(id = 1L), "/baz/1")
    expect_identical(eps$baz(name = "foo"), "/baz/foo")

    # queries & anchors work
    expect_identical(
        eps$baz(name = "foo", .query = list(a = "foo")),
        "/baz/foo?a=foo"
    )
    expect_identical(
        eps$baz(name = "foo", .query = list(a = "foo"), .anchor = "baz"),
        "/baz/foo?a=foo#baz"
    )

    # collision
    suppressMessages({
        api <- plumber2::api() |>
            plumber2::api_get("/foo/:id/bar", function() "foo") |>
            plumber2::api_get("/foo/bar/:id", function() "foo2")
    })

    expect_error(
        endpoints("default", api = api, refresh = TRUE),
        class = "freshwater_endpoint_error"
    )
})
