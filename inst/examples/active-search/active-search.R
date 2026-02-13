library(freshwater)
register_html_serialiser()


# ~~~~~~~~~~ #
# Data setup #
# ###########

df <- mtcars
df$model <- rownames(df)

search_cars <- function(query, limit = 20) {
  q <- trimws(query %||% "")

  if (!nzchar(q)) return(lapply(seq_len(nrow(df)), \(i) {
    list(model = df$model[i], mpg = df$mpg[i], cyl = df$cyl[i])
  }))

  q <- tolower(q)
  hay <- tolower(paste(df$model, df$mpg, df$cyl, sep = " "))
  keep <- grepl(q, hay, fixed = TRUE)

  out <- df[keep, , drop = FALSE]
  if (nrow(out) > limit) out <- out[seq_len(limit), , drop = FALSE]

  lapply(seq_len(nrow(out)), \(i) {
    list(model = out$model[i], mpg = out$mpg[i], cyl = out$cyl[i])
  })
}

# ~~~~~~~~~ #
# Templates #
# ~~~~~~~~~ #

layout <- template(title = "App", {
    htmltools::tagList(
        head(
            title(title),
            script(src = "https://unpkg.com/htmx.org@1.9.12")
        ),
        body(...)
    )
})

search_page <- template(query = "", {
    rows <- search_cars(query)

    layout(
        title = "Search Cars",
        h3(
            "Search Cars ",
            span(
                class = "htmx-indicator",
                img(src = "/img/bars.svg", alt = ""),
                " Searching..."
            )
        ),

        input(
            class = "form-control",
            type = "search",
            name = "search",
            placeholder = "Begin typing to search cars...",
            hx_post = "/search",
            hx_trigger = "input changed delay:100ms, load",
            hx_target = "#search-results",
            hx_indicator = ".htmx-indicator"
        ),

        table(
            class = "table",
            thead(
                tr(
                    th("Model"),
                    th("Mpg"),
                    th("Cyl")
                )
            ),
            tbody(
                id = "search-results",
                cache(
                    name = "car_rows",
                    vary = query,
                    fragment(
                        name = "rows",
                        htmltools::tagList(
                            if (!length(rows)) {
                                tr(td(colspan = 3, em("No results")))
                            } else {
                                lapply(rows, \(r) {
                                    tr(
                                        td(r$model),
                                        td(r$mpg),
                                        td(r$cyl)
                                    )
                                })
                            }
                        )
                    )
                )
            )
        )
    )
})


# ~~~~~~ #
# Routes #
# ~~~~~~ #

#' @get /cars
#' @serializer html
function() {
    search_page()
}

#' @post /search
#' @serializer html
function(body, request) {
    q <- body$search %||% ""
    search_page(query = q, fragment = "rows")
}
