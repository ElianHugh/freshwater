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

})


# test_that("fragments can contain caches", {
#     t1 <- template(x = 1L, {
#         div(
#             fragment(
#                 name = "test",
#                 cache(
#                     "test",
#                     vary = x,
#                     span("foo")
#                 )
#             )
#         )
#     })
# })

# test_that("caches can produce fragments", {
#     t1 <- template(x = 1L, {
#         div(
#             cache(
#                 name = "test",
#                 vary = x,
#                 fragment(
#                     name = "test",
#                     span("foo")
#                 )
#             )
#         )
#     })

#     expect_identical_when_rendered(

#     )
# })
