library(freshwater)

register_html_serialiser()

dashboard_data <- local({
    data <- list(
        users = 3L,
        version = 1L
    )

    list(
        get = \() data,
        version = \() data$version,
        increment_users = \() {
            data$users <<- data$users + 1L
            data$version <<- data$version + 1L
            invisible(TRUE)
        }
    )
})

dashboard <- template(users, version, {
    div(
        h1("Dashboard"),
        p("Active users: ", users),
        p("Version: ", version),
        form(
            action = "/users/increment",
            method = "post",
            button("Add user")
        ),
        form(
            action = "/",
            method = "get",
            button("Refresh")
        )
    )
})

#' @get /
#' @etag dashboard_data$version
#' @serializer html
function() {
    d <- dashboard_data$get()
    dashboard(d$users, d$version)
}

#' @post /users/increment
function(response) {
    dashboard_data$increment_users()
    response$status <- 303L
    response$set_header("Location", "/")
    plumber2::Break
}
