library(freshwater)

register_html_serialiser()

#' @plumber
function(api) {
    api |>
        freshwater::api_error_pages()
}

contacts <- list(
    lis
)

contact_card <- template(contact, {
    div(
        h3(contact$name),
        p(contact$phone),
        p(contact$address)
    )
})


#' @get /
#' @serializer html
function() {
    my_form()
}
