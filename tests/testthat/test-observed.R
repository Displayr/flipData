context("Observed")

test_that("Observed", {

    expect_error(Observed(z <- 1))
    y <- runif(5)
    x <- runif(5)
    ols <- lm(y ~ x)
    expect_equal(Observed(ols), y)
})
