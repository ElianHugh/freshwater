test_that("template caching works", {
    t1 <- template(x = 1L, {
        cache(
            name = "test",
            vary = x,
            span(Sys.time())
        )
    })

    res1 <- t1()
    res2 <- t1()

    Sys.sleep(0.1)
    invalidate_cache(t1, "test", 1L)
    res3 <- t1()
    Sys.sleep(0.1)
    res4 <- t1(x = 2L)

    expect_identical_when_rendered(res1, res2)
    expect_not_identical_when_rendered(res1, res3)
    expect_not_identical_when_rendered(res3, res4)

    # Contextual cache invalidation
    t1 <- template(x = 1L, {
        if (x > 1L) {
            invalidate_cache_here("test", NULL)
        }
        cache(
            name = "test",
            vary = NULL,
            span(Sys.time())
        )
    })

    res1 <- t1()
    res2 <- t1()
    res3 <- t1(2L)
    expect_identical_when_rendered(res1, res2)
    expect_not_identical_when_rendered(res1, res3)

    # Purge cache
    t1 <- template(x = 1L, {
        cache(
            name = "test",
            vary = x,
            span(Sys.time())
        )
    })
    res1 <- t1()
    res2 <- t1()
    clear_cache()
    res3 <- t1()

    expect_identical_when_rendered(res1, res2)
    expect_not_identical_when_rendered(res1, res3)
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

  expect_equal(sum(grepl("cached!", msgs1, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("cached!", msgs2, fixed = TRUE)), 0L)
  expect_equal(sum(grepl("cached!", msgs3, fixed = TRUE)), 1L)
})

test_that("fragments can contain caches", {
    t1 <- template(x = 1L, {
        div(
            fragment(
                name = "test",
                cache(
                    "test",
                    vary = x,
                    span(Sys.time())
                )
            )
        )
    })

    res1 <- t1()
    res2 <- t1()
    Sys.sleep(0.1)
    res3 <- t1(x = 2L)


    expect_identical_when_rendered(res1, res2)
    expect_not_identical_when_rendered(res1, res3)


    res1 <- t1(fragment = "test")
    res2 <- t1(fragment = "test")
    Sys.sleep(0.1)
    res3 <- t1(x = 2L, fragment = "test")

    expect_identical_when_rendered(res1, res2)
    expect_not_identical_when_rendered(res1, res3)
})

test_that("caches can produce fragments", {
    t1 <- template(x = 1L, {
        div(
            cache(
                name = "test",
                vary = x,
                fragment(
                    name = "test",
                    span(Sys.time())
                )
            )
        )
    })

    res1 <- t1(fragment = "test")
    res2 <- t1(fragment = "test")
    Sys.sleep(0.1)
    res3 <- t1(x = 2, fragment = "test")

    expect_identical_when_rendered(res1, res2)
    expect_not_identical_when_rendered(res1, res3)


})


test_that("caches can be nested", {
  page_runs  <- 0L
  stats_runs <- 0L

  dashboard <- template(page, stats, {
    cache(
      "page",
      vary = page$updated_at,
      div(
        { page_runs  <<- page_runs + 1L; NULL },
        h1("Dashboard"),
        cache(
          "stats",
          vary = stats$updated_at,
          p({
            stats_runs <<- stats_runs + 1L
            stats$count
          })
        )
      )
    )
  })

  page1  <- list(updated_at = 1)
  stats1 <- list(updated_at = 10, count = 5)

  out1 <- dashboard(page1, stats1)
  expect_equal(page_runs, 1L)
  expect_equal(stats_runs, 1L)
  expect_match(as.character(out1), ">5<")

  out2 <- dashboard(page1, stats1)
  expect_equal(page_runs, 1L)
  expect_equal(stats_runs, 1L)

  page2 <- list(updated_at = 2)
  out3 <- dashboard(page2, stats1)
  expect_equal(page_runs, 2L)
  expect_equal(stats_runs, 1L)
  expect_match(as.character(out3), ">5<")

  stats2 <- list(updated_at = 11, count = 9)
  page3  <- list(updated_at = 3)
  out4 <- dashboard(page3, stats2)
  expect_equal(page_runs, 3L)
  expect_equal(stats_runs, 2L)
  expect_match(as.character(out4), ">9<")
})
