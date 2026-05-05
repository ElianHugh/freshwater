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


    # you can extract multiple fragments
    t1 <- template({
      div(
        fragment(name = "foo", p("foo")),
        fragment(name = "bar", p("bar")),
        fragment(name = "baz", p("baz")),
      )
    })

    expect_identical_when_rendered(
      t1(fragment = c("foo", "baz")),
      htmltools::tagList(
        htmltools::p("foo"),
        htmltools::p("baz")
      )
    )

     expect_identical_when_rendered(
      t1(fragment = c("baz", "foo")),
      htmltools::tagList(
        htmltools::p("baz"),
        htmltools::p("foo")
      )
    )

    expect_error(
      t1(fragment = c("baz", "foo", "test")),
      class = "freshwater_template_error"
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
      data_attribute = "foo",
      data__attribute = "bar",
      `_data_attribute` = "baz"
    )
  })

  expect_identical_when_rendered(
    t(),
    htmltools::div(
      `data-attribute` = "foo",
      data_attribute = "bar",
      `_data-attribute` = "baz"
    )
  )

  t <- template({
    div(
      span(data_attribute_foo = "bar")
    )
  })

  expect_identical_when_rendered(
    t(),
    htmltools::div(
      htmltools::span(`data-attribute-foo` = "bar")
    )
  )

  t <- template({
    div(
      lapply(seq(1), \(i) div(foo_bar = 1))
    )
  })

  expect_identical_when_rendered(
    t(),
    htmltools::div(
      htmltools::div(`foo-bar` = "1")
    )
  )

  # norming works w/ cache
  t <- template({
    cache(
      name = "test",
      vary = NULL,
      NULL,
      div(data_class_id = 1L)
    )
  })
  expect_identical_when_rendered(
    t(),
      htmltools::div(`data-class-id` = "1")
  )

})


test_that("template scoping works", {
  # foo must come from tmpl env, not caller
  tmpl <- template({
    div(foo)
  })

  expect_error(
    local({
      foo <- "oops"
      tmpl()
    }),
    regexp = "object 'foo' not found"
  )

  # template arguments are resolved correctly

  tmpl <- template(x, {
    div(x)
  })

  res <- local({
    x <- "foo"
    tmpl("bar")
  })

  expect_identical_when_rendered(
    res,
    htmltools::div("bar")
  )

  # use helpers from its defining scope, not
  # the caller scope

  e <- new.env(parent = baseenv())
  e$foo <- function() "foo"
  tmpl <- template(
    {
      div(foo())
    },
    .envir = e
  )

  res <- local({
    foo <- function() "foo-caller"
    tmpl()
  })

  expect_identical_when_rendered(
    res,
    htmltools::div("foo")
  )


  # rendering & definition works even when the env is locked

  e <- new.env(parent = baseenv())
  e$foo <- "ok"
  tmpl <- template({ div(foo) }, .envir = e)
  lockEnvironment(e, bindings = TRUE)
  res <- tmpl()

  expect_identical_when_rendered(
    res,
    htmltools::div("ok")
  )

})

test_that("error traces are maintained", {
  tmpl <- template(x, {
    div(x)
  })


  tmpl_parent <- template({
    tmpl(base::stop("error"))
  })


  expect_snapshot_error(
    tmpl(base::stop("error")),
    class = "freshwater_template_error"
  )

  expect_snapshot_error(
    tmpl_parent(),
    class = "freshwater_template_error"
  )

})

test_that("single root template IDs work", {
  # single root
  tpl <- template(.id = "foo", {
    div()
  })

  expect_identical_when_rendered(
    tpl(),
    htmltools::div(id = "foo")
  )

  tpl <- template(x, y, .id = function(x) x$id, {
    div(.part = "foo")
  })

  expect_identical_when_rendered(
    tpl(x = list(id = 1)),
    htmltools::div(id = 1, `data-fw-part`="1-foo")
  )


  # default args
  tpl <- template(x = list(id = 1L), y, .id = function(x) x$id, {
    div(.part = "foo")
  })

  expect_identical_when_rendered(
      tpl(),
      htmltools::div(id = 1, `data-fw-part` = "1-foo")
  )
})

test_that("multi-root template IDs work", {
  # multi root
  tpl <- template(.id = "foo", {
    htmltools::tagList(
      div(),
      span()
    )
  })
  expect_identical_when_rendered(
    tpl(),
    htmltools::tagList(
      htmltools::div(),
      htmltools::span()
    )
  )

  tpl <- template(.id = "foo", {
    htmltools::tagList(
      div(.part = "bar"),
      span(.part = "baz")
    )
  })
  expect_identical_when_rendered(
    tpl(),
    htmltools::tagList(
      htmltools::div(`data-fw-part` = "foo-bar"),
      htmltools::span(`data-fw-part` = "foo-baz")
    )
  )

  tpl <- template(x, y, .id = function(x) x$id, {
    htmltools::tagList(
      div(.part = "foo"),
      span(.part = "bar")
    )
  })
  expect_identical_when_rendered(
    tpl(x = list(id = 1L)),
    htmltools::tagList(
      htmltools::div(`data-fw-part` = "1-foo"),
      htmltools::span(`data-fw-part` = "1-bar")
    )
  )

  tpl <- template(x = list(id = 1L), y, .id = function(x) x$id, {
    htmltools::tagList(
      div(.part = "foo"),
      span(.part = "bar")
    )
  })
  expect_identical_when_rendered(
    tpl(),
    htmltools::tagList(
      htmltools::div(`data-fw-part` = "1-foo"),
      htmltools::span(`data-fw-part` = "1-bar")
    )
  )
})

test_that("target works", {
  tpl <- template(x, y, .id = function(x) x$id, {
    div(.part = "foo")
  })

  expect_identical(
    target(tpl, x = list(id = 1)),
    "#1"
  )

  tpl <- template(x, y, .id = function(x) x$id, {
    htmltools::tagList(
      div(.part = "foo"),
      span(.part = "bar")
    )
  })

  expect_identical(
    target(tpl, list(id = 1L), .part = "foo"),
    "[data-fw-part=\"1-foo\"]"
  )

  # default args
  tpl <- template(x = list(id = 1L), y, .id = function(x) x$id, {
    div(.part = "foo")
  })

  expect_identical(
    target(tpl),
    "#1"
  )

  expect_identical(
    target(tpl, .part = "foo"),
    "[data-fw-part=\"1-foo\"]"
  )

})