expect_identical_when_rendered <- function(arg1, arg2) {
    arg1 <- arg1 |>
        htmltools::as.tags() |>
        htmltools::doRenderTags()

    arg2 <- arg2 |>
        htmltools::as.tags() |>
        htmltools::doRenderTags()

    expect_identical(arg1, arg2)
}

expect_not_identical_when_rendered <- function(arg1, arg2) {
    arg1 <- arg1 |>
        htmltools::as.tags() |>
        htmltools::doRenderTags()

    arg2 <- arg2 |>
        htmltools::as.tags() |>
        htmltools::doRenderTags()

    expect_false(identical(arg1, arg2))
}