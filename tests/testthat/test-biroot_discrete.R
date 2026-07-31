test_that("biroot_discrete() leaves the RNG state alone and ids are reproducible", {
  f <- function(v) ifelse(v$x^2 + v$y^2 < 1, "in", "out")

  set.seed(42)
  r_untouched <- runif(1)
  set.seed(42)
  out1 <- biroot_discrete(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 4)
  r_after_call <- runif(1)
  expect_identical(r_untouched, r_after_call)

  out2 <- biroot_discrete(f, xlim = c(-2, 2), ylim = c(-2, 2), max_depth = 4)
  expect_identical(out1$id, out2$id)
  expect_identical(out1, out2)
})
