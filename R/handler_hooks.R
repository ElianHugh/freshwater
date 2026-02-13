add_hook <- function(handler, hook) {
    incoming <- if (is.list(hook)) hook else list(hook)

    current <- attr(handler, "freshwater_hooks", exact = TRUE)
    if (is.null(current)) current <- list()

    attr(handler, "freshwater_hooks") <- unique(c(current, incoming))

    handler
}

invoke_hooks <- function(handler) {
    force(handler)

    fn <- function(...) {
        args <- list(...)
        i <- 0L
        next_call <- \() {
            hooks <- attr(handler, "freshwater_hooks", exact = TRUE)
            if (is.null(hooks)) hooks <- list()

            i <<- i + 1L
            if (i <= length(hooks)) {
                return(hooks[[i]](args, next_call))
            }

            do.call(handler, args)
        }
        next_call()
    }

    attr(fn, "freshwater_hook_wrapper") <- TRUE
    attr(fn, "freshwater_hook_base") <- handler
    fn
}

patch_plumber_handler <- function(plumber_handler, hooks) {
    plumber_env <- environment(plumber_handler)
    user_function <- plumber_env[["handler"]]

    if (!is.function(user_function)) {
        return(plumber_handler)
    }

    if (isTRUE(attr(user_function, "freshwater_hook_wrapper", exact = TRUE))) {
        base <- attr(user_function, "freshwater_hook_base", exact = TRUE)
        if (is.function(base)) {
            user_function <- base
        }
    }

    user_function <- add_hook(user_function, hooks)

    # if (
    #     !isTRUE(attr(
    #         plumber_env[["handler"]],
    #         "freshwater_hook_wrapper",
    #         exact = TRUE
    #     ))
    # ) {
    #     plumber_env[["handler"]] <- invoke_hooks(user_function)
    # }
    plumber_env[["handler"]] <- invoke_hooks(user_function)

    plumber_handler
}

enhook_routes <- function(api, hooks) {
    rr <- api$request_router
    routes <- rr$routes
    for (route in routes) {
        r <- rr$get_route(route)
        r$remap_handlers(function(method, path, handler) {
            handler <- patch_plumber_handler(handler, hooks)
            r$add_handler(method, path, handler)
        })
    }
    api
}
