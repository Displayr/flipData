context("warnings")


test_that("CheckForUniqueVariableNames",
{
     expect_error(CheckForUniqueVariableNames(y ~ x + y + y))
     expect_error(CheckForUniqueVariableNames(y ~ x), NA)
})
