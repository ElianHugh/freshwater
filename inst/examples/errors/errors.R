library(freshwater)

register_html_serialiser()

#' @plumber
function(api) {
    api |>
        freshwater::api_error_pages()
}

contacts <- list(
    list(name = "Jim", phone = 123456789, address = "12 Grove Street"),
    list(name = "Sally", phone = 123456788, address = "3 Villa Avenue"),
    list("MALFORMED LIST")
)

contact_card <- template(contact, {
    if (nzchar(contact$name)) {
        div(
            h3(contact$name),
            p(contact$phone),
            p(contact$address)
        )
    }
})

contact_cards <- template(contacts, {
    htmltools::tagList(
        lapply(contacts, function(contact) contact_card(contact))
    )
})


page <- template(contacts, {
    body(
        h1("Contacts"),
        contact_cards(contacts = contacts)
    )
})

#' @get /
#' @serializer html
function() {
    page(contacts = contacts)
}
