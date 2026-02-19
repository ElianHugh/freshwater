get_user_handler_from_route <- function(api, path) {
	rr <- api$request_router
	r <- rr$get_route("default")
	e <- environment(r$get_handler("get", path))
	attr(e$handler, "freshwater_hook_base", exact = TRUE)
}

test_that("error page installation is idempotent", {
	api <- plumber2::api()
	api <- plumber2::api_get(api, "/", function() {
        	"foo"
	})

	api <- api_error_pages(api)
	api$trigger("freshwater_error_pages")
	handler <- get_user_handler_from_route(api, "/")
	hooks <- attr(handler, "freshwater_hooks", exact = TRUE)
	expect_length(hooks, 1L)

	api <- api_error_pages(api)
	api$trigger("freshwater_error_pages")
	handler <- get_user_handler_from_route(api, "/")
	hooks <- attr(handler, "freshwater_hooks", exact = TRUE)
	expect_length(hooks, 1L)
})


test_that("error pages render upon user error", {
	testthat::skip_if_not_installed("cli")

	api <- plumber2::api()
	api <- plumber2::api_get(api, "/foo", function() {
		"foo"
	})
	api <- plumber2::api_get(api, "/bar", function() {
		stop("bar")
	})
	api <- api_error_pages(api, debug = TRUE)
	api$trigger("freshwater_error_pages")

	# 200

	req <- fiery::fake_request(
		"http://localhost:8080/foo",
		headers = list(
			accept = "text/html; charset=utf-8"
		),
		method = "get"
	)
	res <- api$test_request(req)
	expect_identical(res$status, 200L)
	expect_identical(res$body, "foo")

	# Error page
	req <- fiery::fake_request(
		"http://localhost:8080/bar",
		headers = list(
			accept = "text/html; charset=utf-8"
		),
		method = "get"
	)
	res <- api$test_request(req)

	error_page <- default_error_500_template()(
		e = errorCondition("bar"),
		is_debug = TRUE
	) |>
		as.character()

	expect_identical(res$status, 500L)
	expect_identical(res$body, error_page)

	# Missing page
	req <- fiery::fake_request(
		"http://localhost:8080/baz",
		headers = list(
			accept = "text/html; charset=utf-8"
		),
		method = "get"
	)
	res <- api$test_request(req)

	missing_page <- default_error_404_template()() |>
		as.character()

	expect_identical(res$status, 404L)
	expect_identical(res$body, missing_page)
})