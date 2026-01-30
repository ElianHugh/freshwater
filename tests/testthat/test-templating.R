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

  expect_identical(
    htmltools::doRenderTags(t1),
    htmltools::doRenderTags(t2)
  )
})


test_that("template fragments work", {
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

  expect_identical(
    htmltools::doRenderTags(t1()),
    htmltools::doRenderTags(t2)
  )

  t3 <- t1(fragment = "test")
  t4 <- htmltools::p("Bar")
  expect_identical(
    htmltools::doRenderTags(t3),
    htmltools::doRenderTags(t4)
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

  expect_identical(
    htmltools::doRenderTags(t(1, 2, 3)),
    htmltools::doRenderTags(
      htmltools::tagList(
        htmltools::p(1),
        htmltools::p(2),
        htmltools::p(3)
      )
    )
  )

})

test_that("template dots work", {
  t <- template({
    div(...)
  })

  expect_no_error(res <- t(1))
  expect_identical(
    htmltools::doRenderTags(res),
    htmltools::doRenderTags(
      htmltools::div(1)
    )
  )
})