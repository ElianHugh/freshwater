library(freshwater)

#' @plumber
function(api) {
    api |>
        api_freshwater(debug = TRUE, csrf = FALSE)
}

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
    document(
        head(
            title(title),
            script(src = "https://unpkg.com/htmx.org@1.9.12")
        ),
        body(...)
    )
})

search_page <- template(query = "", .id = "table", {
    rows <- search_cars(query)

    htmltools::tagList(
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
            hx_post = endpoints("active-search")$search$post(),
            hx_trigger = "input changed delay:100ms, load",
            hx_target = target(search_page, .part = "search-results"),
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
                .part = "search-results",
                cache(
                    name = "car_rows",
                    vary = query,
                    ttl = NULL,
                    fragment(
                        name = "rows",
                        map_tags(
                            rows,
                            function(r) {
                                tr(
                                    td(r$model),
                                    td(r$mpg),
                                    td(r$cyl)
                                )
                            },
                            .empty = tr(td(colspan = 3, em("No results")))
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

#' @get /
function(response) {
    redirect(response, "/cars")
}

#' @get /cars
#' @serializer html
function() {
    layout(
        title = "Search Cars",
        search_page()
    )
}

#' @post /search
#' @serializer html
function(body, request) {
    q <- body$search %||% ""
    search_page(query = q, fragment = "rows")
}
