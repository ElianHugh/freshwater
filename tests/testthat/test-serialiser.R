test_that("serialisation renders non-body tags", {
    register_html_serialiser()
    fn <- plumber2::get_serializers("html")[[1L]]

    tpl <- template({
        document(
            head(meta(title = "foo")),
            body(div("bar"))
        )
    })

    expect_identical(
        fn(tpl()),
        fn(htmltools::tagList(
            htmltools::HTML("<!DOCTYPE html>"),
            htmltools::tags$html(
                htmltools::tags$head(
                    htmltools::tags$meta(title = "foo")
                ),
                htmltools::tags$body(
                    htmltools::div("bar")
                )
            )
        ))
    )

})
