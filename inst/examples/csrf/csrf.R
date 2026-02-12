library(freshwater)

register_html_serialiser()

#' @plumber
function(api) {
    api |>
        freshwater::api_csrf(secure = FALSE)
}

my_form <- template({
    div(
        h2("Allowed"),
        form(
            action = "/post",
            method = "post",
            input(type = "submit")
        ),
        h2("Disallowed"),
        htmltools::tags$form(
            action = "/post",
            method = "post",
            input(type = "submit")
        )
    )

})

#' @get /
#' @serializer html
function() {
    my_form()
}

#' @post /post
function(response) {
    print("Authenticated!")
    redirect(response, "/")
}
