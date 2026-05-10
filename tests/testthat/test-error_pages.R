get_user_handler_from_route <- function(api, path) {
	rr <- api$request_router
	r <- rr$get_route("default")
	e <- environment(r$get_handler("get", path))
	attr(e$handler, "freshwater_hook_base", exact = TRUE)
}

test_that("error page installation is idempotent", {
	suppressMessages({
		api <- plumber2::api()
		api <- plumber2::api_get(api, "/", function() {
			"foo"
		})
	})


	api <- api_error_pages(api)
	api$trigger("freshwater::hook")
	handler <- get_user_handler_from_route(api, "/")
	hooks <- attr(handler, "freshwater_hooks", exact = TRUE)
	expect_length(hooks, 2L)

	api <- api_error_pages(api)
	api$trigger("freshwater::hook")
	handler <- get_user_handler_from_route(api, "/")
	hooks <- attr(handler, "freshwater_hooks", exact = TRUE)
	expect_length(hooks, 2L)
})

test_that("error pages render upon user error", {
	testthat::skip_if_not_installed("cli")

	suppressMessages({
		api <- plumber2::api()
		api <- plumber2::api_get(api, "/foo", function() {
			"foo"
		})
		api <- plumber2::api_get(api, "/bar", function() {
			stop("bar")
		})
	})

	api <- api_freshwater(api, debug = TRUE, csrf=FALSE)
	api$trigger("freshwater::hook")

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

	error_page <- default_error_500_template(
		e = errorCondition("bar"),
		is_debug = TRUE
	) |>
		htmltools::doRenderTags() |>
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

	missing_page <- default_error_404_template() |>
		htmltools::doRenderTags() |>
		as.character()

	expect_identical(res$status, 404L)
	expect_identical(res$body, missing_page)
})

test_that("manual user 404s are respected", {
	testthat::skip_if_not_installed("cli")

	suppressMessages({
		api <- plumber2::api()
		api <- plumber2::api_get(api, "/foo", function(response) {
			response$status <- 404L
			response$body <- "Foo"
		})
		api <- api_freshwater(api, debug = TRUE, csrf =FALSE)
	})

	api$trigger("freshwater::hook")

	req <- fiery::fake_request(
		"http://localhost:8080/foo",
		headers = list(
			accept = "text/html; charset=utf-8"
		),
		method = "get"
	)
	res <- api$test_request(req)

	missing_page <- default_error_404_template(list(
		response = list(body = "Foo")
	)) |>
		htmltools::doRenderTags() |>
		as.character()

	expect_identical(res$status, 404L)
	expect_identical(res$body, missing_page)
})

test_that("other methods return error pages", {
	testthat::skip_if_not_installed("cli")

	suppressMessages({
		api <- plumber2::api()
		api <- plumber2::api_post(api, "/foo", function() {
			"foo"
		})
		api <- plumber2::api_post(api, "/bar", function() {
			stop("bar")
		})
		api <- api_freshwater(api, debug = TRUE, csrf=FALSE)
	})

	api$trigger("freshwater::hook")


	# 500L

	req <- fiery::fake_request(
		"http://localhost:8080/bar",
		headers = list(
			accept = "text/html; charset=utf-8"
		),
		method = "post"
	)
	res <- api$test_request(req)
	error_page <- default_error_500_template(
		e = errorCondition("bar"),
		is_debug = TRUE
	) |>
		htmltools::doRenderTags() |>
		as.character()

	expect_identical(res$status, 500L)
	expect_identical(res$body, error_page)

	# 404

	req <- fiery::fake_request(
		"http://localhost:8080/baz",
		headers = list(
			accept = "text/html; charset=utf-8"
		),
		method = "post"
	)
	res <- api$test_request(req)

	missing_page <- default_error_404_template() |>
		htmltools::doRenderTags() |>
		as.character()

	expect_identical(res$status, 404L)
	expect_identical(res$body, missing_page)

})

test_that("error pages only occur for HTML", {
	testthat::skip_if_not_installed("cli")

	suppressMessages({
		api <- plumber2::api()
		api <- plumber2::api_get(api, "/foo", function() {
			"foo"
		})
		api <- plumber2::api_get(api, "/bar", function() {
			stop("bar")
		})
		api <- api_freshwater(api, debug = TRUE, csrf=FALSE)
	})


	api$trigger("freshwater::hook")

	# Error page
	req <- fiery::fake_request(
		"http://localhost:8080/bar",
		headers = list(
			accept = "application/json"
		),
		method = "get"
	)
	res <- api$test_request(req)
	expect_identical(res$status, 500L)
	expect_identical(res$body, "[\"Internal Server Error\"]")

	# Missing page
	req <- fiery::fake_request(
		"http://localhost:8080/baz",
		headers = list(
			accept = "application/json"
		),
		method = "get"
	)
	res <- api$test_request(req)
	expect_identical(res$status, 404L)
	expect_identical(res$body, "Not Found")

})


test_that("error pages function for `then` handlers", {
	suppressMessages({
		api <- plumber2::api()
		api <- plumber2::api_get(
			api,
			"/",
			function() {
				"foo"
			},
			async = TRUE,
			then = list(
				function() {
					TRUE
				},
				function() {
					stop("Bad handler")
				}
			)
		)

		api <- api_freshwater(api, debug = TRUE, csrf=FALSE)
	})

	api$trigger("freshwater::hook")

	res <- faux_request(api, accept = "text/html; charset=utf-8") |>
		wait_for_resolve()

	expect_identical(res$status, 500L)
	expect_match(res$body, "Bad handler", fixed = TRUE)
})

test_that("error pages work when cget is present", {
	suppressMessages({
		api <- plumber2::api()
		api <- plumber2::api_get(api, "/", \() stop("Foo"))
		api <- api_cget(api, "/", \() 1L)
		api <- api_freshwater(api, debug = TRUE, csrf = FALSE)
		api$trigger("freshwater::hook")
	})

	res <- faux_request(api, path = "/", accept = "text/html; charset=utf-8")
	expect_true(is.null(res$headers$etag))
})

test_that("errors work under async", {
	testthat::skip_if_not_installed("cli")
	testthat::skip_if_not_installed("mori")
	testthat::skip_if_not_installed("mirai")
	testthat::skip_if_not_installed("promises")


	suppressMessages({
		register_async_evaluator()
		api <- plumber2::api()
		api <- plumber2::api_get(api, "/internal", \() plumber2::abort_internal_error("Internal Error"), async = "freshwater")
		api <- plumber2::api_get(api, "/forbidden", \() plumber2::abort_forbidden("Forbidden"), async = "freshwater")
		api <- plumber2::api_get(api, "/not_found", \() plumber2::abort_not_found("Not Found"), async = "freshwater")
		api <- api_freshwater(api, debug = TRUE, csrf = FALSE)
		api$trigger("freshwater::hook")

		res <- faux_request(api, path = "internal", accept = "text/html; charset=utf-8") |>
			wait_for_resolve()
		expect_identical(res$status, 500L)
		expect_match(res$body, "Internal Error", fixed = TRUE)

		res <- faux_request(api, path = "forbidden", accept = "text/html; charset=utf-8") |>
			wait_for_resolve()
		expect_identical(res$status, 403L)
		expect_match(res$body, "Forbidden", fixed = TRUE)


		res <- faux_request(api, path = "not_found", accept = "text/html; charset=utf-8") |>
			wait_for_resolve()
		expect_identical(res$status, 404L)
		expect_match(res$body, "Not Found", fixed = TRUE)
	})
})

test_that("custom error pages work", {
	suppressMessages({
		api <- plumber2::api()
		api <- plumber2::api_get(api, "/foo", function() {
			"foo"
		})
		api <- plumber2::api_get(api, "/bar", function() {
			stop("bar")
		})
	})

	api <- api_freshwater(api, debug = TRUE, csrf = FALSE, handlers = list(
		"500" = template(error = NULL, request = NULL, is_debug = FALSE, {
			div(
				"Error 500",
				p(paste0(current_path(), " threw an error!"))
			)
		})
	))
	api$trigger("freshwater::hook")

	res <- faux_request(
		api,
		"bar",
		accept = "text/html; charset=utf-8"
	)

	expect_identical(res$status, 500L)
	expect_match(res$body, "/bar threw an error!")
})