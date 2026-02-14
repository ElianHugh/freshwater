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
})