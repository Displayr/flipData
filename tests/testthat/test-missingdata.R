context("Missing Data")
data(bank, package = "flipExampleData")

dat <- data.frame(a = rep((1:10)/10,2),
    b = rep(1:10,2),
    c = factor(rep(c(rep("A",5), rep("B",5)),2)),
    d = ordered(rep(c(rep("A",5), rep("B",5)),2)), e = rep("dog",20), stringsAsFactors = FALSE)
for (i in 1:5)
    dat[i, i] <- NA

test_that("Missing options",
{
    expect_error(EstimationData(Overall ~ Branch, bank, missing = "Error if missing data")$estimation.data)
    expect_equal(341, suppressWarnings(nrow(EstimationData(Overall ~ Branch, bank, missing = "Exclude cases with missing data")$estimation.data)))
    expect_equal(823, nrow(EstimationData(Overall ~ Branch, bank, missing = "Use partial data")$estimation.data))
    expect_equal(823, nrow(EstimationData(Overall ~ Branch, bank, missing = "Use partial data (pairwise correlations)")$estimation.data))
})

test_that("Infinity",
{
    expect_error(ErrorIfInfinity(bank), NA)
    expect_error(ErrorIfInfinity(dat), NA)
    dat$a[10] <- Inf
    dat$b[10] <- -Inf
    expect_error(ErrorIfInfinity(dat),
                 "Variable(s) a, b contain infinite values. Either recode the infinities to finite values or set them as missing data.",
                 fixed = TRUE)

})

no.missing.df <- data.frame(Y = 1:2, X1 = 1:2, X2 = 1:2)

outcome.has.missing.df <- data.frame(Y = c(NA, 1), X1 = 1:2, X2 = 1:2)
expected.missing.outcome <- data.frame(Y = 1, X1 = 2, X2 = 2, row.names = 2L)
expected.missing.outcome.unchecked <- data.frame(Y = c(NA, 1), X1 = c(1, 2), X2 = c(1, 2))

single.pred.missing <- data.frame(Y = 1:2, X1 = c(NA, 2), X2 = c(1, 2))
expected.single <- data.frame(Y = 1:2, X1 = c(0, 2), X2 = c(1, 2), X1.dummy.var = c(1, 0))

diag.missing <- data.frame(Y = 1:2, X1 = c(NA, 2), X2 = c(1, NA))
diag.missing.dummy <- data.frame(Y = 1:2, X1 = c(0, 2), X2 = c(1, 0), X1.dummy.var = 1:0, X2.dummy.var = 0:1)

missing.predictors.case.df <- data.frame(Y = 1:2, X1 = c(NA, 2), X2 = c(NA, 2))
expected.missing.predictors <- structure(list(Y = 2L, X1 = 2, X2 = 2), row.names = 2L, class = "data.frame")
expected.missing.predictors.unchecked <- data.frame(Y = 1:2, X1 = c(0, 2), X2 = c(0, 2),
                                                    X1.dummy.var = 1:0, X2.dummy.var = 1:0)

larger.case.with.missing.df <- data.frame(Y  = c(1, 2,  3,  4, 5, NA, 1, 2,  1),
                                          X1 = c(1, 4, NA,  3, 2,  3, 1, 0, NA),
                                          X2 = c(5, 4,  6, NA, 1,  3, 4, 0, NA))
expected.larger.case <- structure(list(Y = c(1, 2, 3, 4, 5, 1, 2),
                                       X1 = c(1, 4, 0, 3, 2, 1, 0),
                                       X2 = c(5, 4, 6, 0, 1, 4, 0),
                                       X1.dummy.var = c(0L, 0L, 1L, 0L, 0L, 0L, 0L),
                                       X2.dummy.var = c(0L, 0L, 0L, 1L, 0L, 0L, 0L)),
                                  row.names = c(1L, 2L, 3L, 4L, 5L, 7L, 8L), class = "data.frame")

test_that("Dummy variable adjustment", {
    expect_identical(AddDummyVariablesForNAs(no.missing.df, outcome.name = "Y"),
                     no.missing.df)
    expect_identical(AddDummyVariablesForNAs(no.missing.df, outcome.name = "Y", checks = FALSE),
                     no.missing.df)
    expect_equal(AddDummyVariablesForNAs(single.pred.missing, outcome.name = "Y"),
                     expected.single)
    expect_equal(AddDummyVariablesForNAs(single.pred.missing, outcome.name = "Y", checks = FALSE),
                     expected.single)

    expect_equal(AddDummyVariablesForNAs(outcome.has.missing.df, outcome.name = "Y"),
                 expected.missing.outcome)
    expect_equal(AddDummyVariablesForNAs(outcome.has.missing.df, outcome.name = "Y", checks = FALSE),
                 expected.missing.outcome.unchecked)
    expect_identical(AddDummyVariablesForNAs(diag.missing, "Y"),
                     diag.missing.dummy)
    expect_identical(AddDummyVariablesForNAs(diag.missing, "Y", checks = FALSE),
                     diag.missing.dummy)
    # Edge case, missing predictors make dummy redundant
    expect_equal(AddDummyVariablesForNAs(missing.predictors.case.df, outcome.name = "Y"),
                 expected.missing.predictors)
    expect_equal(AddDummyVariablesForNAs(missing.predictors.case.df, outcome.name = "Y", checks = FALSE),
                 expected.missing.predictors.unchecked)
    expect_identical(AddDummyVariablesForNAs(larger.case.with.missing.df, outcome.name = "Y"),
                     expected.larger.case)
    expect_identical(AddDummyVariablesForNAs(larger.case.with.missing.df, outcome.name = "Y", checks = TRUE),
                     expected.larger.case)
})
