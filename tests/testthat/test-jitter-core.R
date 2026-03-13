testthat::test_that("bounded CV jitter changes value and stays interior", {
  source(file.path("..", "..", "tools", "jitter.R"))

  set.seed(42)
  x <- replicate(100, jitter_sample_bounded_cv(current_val = 0.5, jitter_cv = 0.2, lower = 0, upper = 1))

  testthat::expect_true(all(is.finite(x)))
  testthat::expect_true(all(x > 0.02 & x < 0.98))
  testthat::expect_true(all(abs(x - 0.5) > 1e-12))
})

testthat::test_that("uniform pct jitter at zero no longer remains fixed", {
  source(file.path("..", "..", "tools", "jitter.R"))

  set.seed(1)
  x <- replicate(100, jitter_sample_uniform_pct(current_val = 0, bound = 0.2, lower = 0, upper = 1))

  testthat::expect_true(all(is.finite(x)))
  testthat::expect_true(all(x >= 0 & x <= 1))
  testthat::expect_true(all(abs(x - 0) > 1e-12))
})

testthat::test_that("dirichlet bounded CV keeps simplex and changes vector", {
  source(file.path("..", "..", "tools", "jitter.R"))

  current <- c(0.2, 0.3, 0.5)
  lower <- c(0.01, 0.01, 0.01)
  upper <- c(0.98, 0.98, 0.98)

  set.seed(99)
  out <- sample_dirichlet_bounded_cv(
    current_vals = current,
    jitter_cv = 0.2,
    lower = lower,
    upper = upper
  )

  testthat::expect_true(all(is.finite(out)))
  testthat::expect_true(all(out >= lower - 1e-12))
  testthat::expect_true(all(out <= upper + 1e-12))
  testthat::expect_equal(sum(out), sum(current), tolerance = 1e-8)
  testthat::expect_true(any(abs(out - current) > 1e-12))
})
