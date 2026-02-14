testthat::skip_if_not_installed("cli")


get_user_handler_from_route <- function(api, path) {
	rr <- api$request_router
	r <- rr$get_route("default")
	e <- environment(r$get_handler("get", path))
	e$handler
}

test_that("error page installation is idempotent", {
	api <- plumber2::api()
	api <- plumber2::api_get(api, "/", function() {
        	"foo"
	})
	
	api <- api_error_pages(api)
	handler <- get_user_handler_from_route(api, "/")
	hooks <- attr(handler, "freshwater_hooks", exact = TRUE)
	
	expect_length(hooks, 1L)

	api <- api_error_pages(api)
	handler <- get_user_handler_from_route(api, "/")
	hooks <- attr(handler, "freshwater_hooks", exact = TRUE)
	expect_length(hooks, 1L)
})
