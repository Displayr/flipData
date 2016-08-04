library(testthat)
context("Weights")


test_that("Effective Sample Size",
{
    expect_equal(EffectiveSampleSize(rep(1,100)), 100)
    expect_equal(EffectiveSampleSize(c(10, rep(1,9))), 3.311927, tolerance = 0.00001)
    expect_error(EffectiveSampleSize(c(Inf, rep(1,9))))
    expect_error(EffectiveSampleSize(c(-1, rep(1,9))))
    expect_error(EffectiveSampleSize(c(NA, rep(1,9))))
})
