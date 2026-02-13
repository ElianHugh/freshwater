library(freshwater)

register_html_serialiser()

#' @plumber
function(api) {
    api |>
        freshwater::api_csrf(secure = TRUE)
}

my_form <- template({
    div(
        h2("Allowed"),
        form(
            action = "/post-form",
            method = "post",
            input(type = "submit")
        ),
        h2("Disallowed"),
        htmltools::tags$form(
            action = "/post-form",
            method = "post",
            input(type = "submit")
        )
    )
})

#'.When working outside of freshwater forms, you can rely on using
#' a meta tag (or other relevant tag) with the csrf token as its value.
#' This is necessary for using the token value in JS injection
my_template <- template({
    js <- r"(
        document.addEventListener('submit', function (e) {
            const form = e.target;

            if (form.method.toLowerCase() === 'post') {
                const token = document
                    .querySelector('meta[name=\"csrf-token\"]')
                    ?.getAttribute('content');

                if (!token) return;

                const input = document.createElement('input');
                input.type = 'hidden';
                input.name = 'csrf_token';
                input.value = token;

                form.appendChild(input);
            }
        });
    )"

    html(
        head(
            meta(
                name = "csrf-token",
                content = csrf_token()
            ),
            script(js)
        ),
        body(
            htmltools::tags$form(
                action = "/post-meta",
                method = "post",
                input(type = "submit")
            )
        )
    )
})

#' @get /form
#' @serializer html
function() {
    my_form()
}

#' @get /meta
#' @serializer html
function() {
    my_template()
}

#' @post /post-form
function(response) {
    print("Authenticated via form body!")
    redirect(response, "/form")
}

#' @post /post-meta
function(response) {
    print("Authenticated via meta tag!")
    redirect(response, "/meta")
}
