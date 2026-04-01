library(freshwater)

#' @plumber
function(api) {
    api |>
        api_freshwater(csrf=T)
}

page <- template({
    div(
        p("Current path is :", current_path()),
        p("Current method is :", current_method())
    )

})

#' @get /
function(response) {
    redirect(
        response,
        endpoints("context")$foo()
    )
}

#' @get /foo
#' @serializer html
function() {
    page()
}