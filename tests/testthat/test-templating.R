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


test_that("templates work", {
  t1 <- template({
    div(
      p("Foo"),
      p("Bar")
    )
  })()

  t2 <- htmltools::div(
    htmltools::p("Foo"),
    htmltools::p("Bar")
  )

  expect_identical_when_rendered(t1, t2)
})


test_that("template fragments work", {
  # Multiple fragments
  t1 <- template({
    div(
      p("Foo"),
      fragment(p("Bar"), name = "test"),
      fragment(p("Baz"), name = "test2")
    )
  })

  t2 <- htmltools::div(
    htmltools::p("Foo"),
    htmltools::p("Bar"),
    htmltools::p("Baz")
  )

  expect_identical_when_rendered(t1(), t2)

  t3 <- t1(fragment = "test")
  t4 <- htmltools::p("Bar")
  expect_identical_when_rendered(t3, t4)

  # Fragments without a root
  t1 <- template({
    div(
      p("Foo"),
      fragment(
        name = "test",
        htmltools::tagList(
          htmltools::p("Bar"),
          htmltools::p("Baz"),
        )
      )
    )
  })

  expect_identical_when_rendered(
    t1(),
    htmltools::div(
      htmltools::p("Foo"),
      htmltools::p("Bar"),
      htmltools::p("Baz")
    )
  )

  # Fragments when children are conditional

  t1 <- template(x, {
    div(
      p("Foo"),
      fragment(
        name = "test",
        htmltools::tagList(
          if (x > 1) {
            htmltools::p("Bar")
          } else {
            htmltools::p("Baz")
          }
        )
      )
    )
  })

  expect_identical_when_rendered(
    t1(x = 2L, fragment = "test"),
    htmltools::p("Bar")
  )

  expect_identical_when_rendered(
    t1(x = 1L, fragment = "test"),
    htmltools::p("Baz")
  )


  # Lists too

    t1 <- template(x, {
      div(
        p("Foo"),
        fragment(
          name = "test",
          htmltools::tagList(
            lapply(seq(10), function(i) htmltools::p(i))
          )
        )
      )
    })

    expect_identical_when_rendered(
      t1(fragment = "test"),
      htmltools::as.tags(lapply(seq(10), function(i) htmltools::p(i)))
    )

})

test_that("template params work", {
  t <- template(a, b, c, {
    htmltools::tagList(
      p(a),
      p(b),
      p(c)
    )
  })

  expect_identical_when_rendered(
    t(1, 2, 3),
    htmltools::tagList(
      htmltools::p(1),
      htmltools::p(2),
      htmltools::p(3)
    )
  )

})

test_that("template dots work", {
  t <- template({
    div(...)
  })
  expect_no_error(res <- t(1))
  expect_identical_when_rendered(
    res,
    htmltools::div(1)
  )
})

test_that("template attribute norming", {
  t <- template({
    div(
      data_attribute="foo",
      data__attribute="bar",
      `_data_attribute`="baz"
    )
  })

  expect_identical_when_rendered(
    t(),
    htmltools::div(
      `data-attribute`="foo",
      data_attribute = "bar",
      `_data-attribute`="baz"
    )
  )


  t <- template({
    div(
      span(data_attribute_foo="bar")
    )
  })

  expect_identical_when_rendered(
    t(),
    htmltools::div(
      htmltools::span(`data-attribute-foo`="bar")
    )
  )
})

test_that("template caching works", {

  t <- template(user, {
    div(
      cache("foo", user$id,
        message("cached!"),
        div()
      )
    )
  })

  msgs1 <- testthat::capture_messages(t(list(id = 1)))
  msgs2 <- testthat::capture_messages(t(list(id = 1)))
  msgs3 <- testthat::capture_messages(t(list(id = 2)))

  expect_true(any(grepl("cached!", msgs1, fixed = TRUE)))
  expect_false(any(grepl("cached!", msgs2, fixed = TRUE)))
  expect_true(any(grepl("cached!", msgs3, fixed = TRUE)))
})
