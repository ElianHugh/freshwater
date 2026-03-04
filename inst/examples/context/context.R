library(freshwater)

#' @plumber
function(api) {
    api |>
        api_freshwater(csrf=T)
}

page <- template({
    div(
        p("Current path is :", current_path())
    )

})

#' @get
function(response) {
    redirect(response, "/foo")
}

#' @get /foo
#' @serializer html
function() {
    page()
}