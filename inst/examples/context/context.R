library(freshwater)

#' @plumber
function(api) {
    api |>
        api_freshwater(csrf=T)
}

page <- template({
    print(current_root())
    print(current_path())
    div(
        p("Project root is :", current_root()),
        p("Current path is :", current_path())
    )

})

#' @get /foo
#' @serializer html
function() {
    page()
}