library(freshwater)

#' @plumber
function(api) {
    register_html_serialiser()
    api |>
        api_csrf(secure = FALSE) |>
        api_error_pages(debug = TRUE)
}

demo <- template(result = "", {
    js <- r"(
        async function sendWithHeader() {
        const out = document.getElementById('result');

        const token = document
            .querySelector('meta[name=\"csrf-token\"]')
            ?.getAttribute('content');

        try {
            const res = await fetch('/post', {
            method: 'POST',
            headers: { 'X-CSRF-Token': token }
            });

            const text = await res.text();

            if (res.ok) {
            out.textContent = 'Authenticated (JS)';
            } else {
            out.textContent = `FAILED (JS): ${res.status}\n${text}`;
            }
        } catch (err) {
            out.textContent = `FAILED (JS): ${err}`;
        }
        }
    )"

    css <- r"(
        body{
            max-width: 720px;
            margin: 24px auto;
            padding: 0 16px;
            font-family: system-ui, -apple-system, "Segoe UI", sans-serif;
            line-height: 1.45;
            color: #1f2937;
        }

        h1 { margin: 0 0 16px; font-size: 24px; }
        h2 { margin: 18px 0 8px; font-size: 13px; font-weight: 650; color: #6b7280; text-transform: uppercase; letter-spacing: .06em; }

        form, button { margin: 0 0 10px; }

        button, input[type="submit"] {
            padding: 10px 12px;
            border-radius: 10px;
            border: 1px solid #d1d5db;
            background: #fff;
            font-weight: 600;
            cursor: pointer;
        }

        button:hover, input[type="submit"]:hover{
            border-color: #94a3b8;
            background: #f8fafc;
        }

        .good_form input[type="submit"], button { border-color: #10b981; }
        .bad_form input[type="submit"]{ border-color: #ef4444; }

        #result {
            margin-top: 10px;
            padding: 12px;
            border-radius: 12px;
            border: 1px solid #e5e7eb;
            background: #f9fafb;
            font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
            font-size: 12.5px;
            white-space: pre-wrap;
        }

    )"


    html(
        head(
            meta(name = "csrf-token", content = csrf_token()),
            script(js),
            style(css)
        ),
        body(
            h1("CSRF Demo"),

            div(
                class = "good_form",
                h2("freshwater form(): POST (auto token)"),
                form(
                    action = "/post",
                    method = "post",
                    input(type = "submit", value = "POST OK")
                )
            ),

            # freshwater provides method spoofing, so using delete,
            # put, and patch works inside HTML forms
            div(
                class = "good_form",
                h2("freshwater form(): DELETE (auto token)"),
                form(
                    action = "/delete",
                    method = "delete",
                    input(type = "submit", value = "DELETE OK")
                )
            ),

            div(
                class = "good_form",
                h2("fetch(): POST (X-CSRF-Token header)"),
                button(
                    onclick = "sendWithHeader()",
                    "POST OK (header)"
                )
            ),

            div(
                class = "bad_form",
                h2("Tokenless POST"),
                htmltools::tags$form(
                    action = "/post",
                    method = "post",
                    input(type = "submit", value = "POST fail")
                ),
            ),

            div(
                class = "panel",
                h2("Result"),
                pre(result, id = "result", class = "result")
            )
        )
    )
})

result <- ""

#' @get /
function(response) {
    redirect(response, "/form")
}

#' @get /form
#' @serializer html
function(response) {
    demo(result = result)
}

#' @post /post
function(response) {
    result <<- "Authenticated (POST)"
    redirect(response, "/form")
}

#' @delete /delete
function(response) {
    result <<- "Authenticated (DELETE)"
    redirect(response, "/form")
}
