library(freshwater)

register_html_serialiser()


tpl <- template({
    div(
        h1("Hello world!"),
        form(
            action = "/post",
            method = "post",
            label("for" = "pick", "Pick a number"),
            input(type="number", name="pick", min=1, max=5),
            br(),
            input(type="submit")
        )
    )
})

cd <- template(n, {
    div(
        h1("Please wait..."),
        p(n)
    )

})

#' @get /
#' @serializer html
function() {
    tpl()
}

#' @post /post
#' @body pick:numeric a number
function(body, response) {
    n <- body$pick
    redirect(response, sprintf("/count/%s", n))
}

#' @get /count/<n>
#' @serializer html
function(n, response) {
    n <- as.integer(n)
    next_n <- n - 1L

    next_url <- if (is.na(next_n) || next_n <= 0L) {
        "/"
    } else {
        sprintf("/count/%d", next_n)
    }

    redirect(response, next_url, after = 1L)
    cd(n)
}
