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
    expect_equal(759, nrow(EstimationData(Overall ~ Fees + Interest, bank, missing = "Dummy variable adjustment")$estimation.data))
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
expected.single <- data.frame(Y = 1:2, X1 = c(2, 2), X2 = c(1, 2), X1.dummy.var_GQ9KqD7YOf = c(1, 0))
attr(expected.single[["X1.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- "X1"
diag.missing <- data.frame(Y = 1:2, X1 = c(NA, 2), X2 = c(1, NA))
diag.missing.dummy <- data.frame(Y = 1:2, X1 = c(2, 2), X2 = c(1, 1), X1.dummy.var_GQ9KqD7YOf = 1:0, X2.dummy.var_GQ9KqD7YOf = 0:1)
attr(diag.missing.dummy[["X1.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- "X1"
attr(diag.missing.dummy[["X2.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- "X2"

missing.predictors.case.df <- data.frame(Y = 1:3, X1 = c(NA, 1, 2), X2 = c(NA, -1, 0))
expected.missing.predictors <- structure(list(Y = 2:3, X1 = c(1, 2), X2 = c(-1, 0)),
                                         row.names = 2:3, class = "data.frame")

expected.missing.predictors.unchecked <- structure(list(Y = 1:3,
                                                        X1 = c(1.5, 1, 2),
                                                        X2 = c(-0.5, -1, 0),
                                                        X1.dummy.var_GQ9KqD7YOf = structure(c(1L, 0L, 0L),
                                                                                            predictors.matching.dummy = c("X1", "X2"))),
                                                   class = "data.frame", row.names = c(NA, -3L))

larger.case.with.missing.df <- data.frame(Y  = c(1, 2,  3,  4, 5, NA, 1, 2,  1),
                                          X1 = c(1, 4, NA,  3, 2,  3, 1, 0, NA),
                                          X2 = c(5, 4,  6, NA, 1,  3, 4, 0, NA))
expected.larger.case <- structure(list(Y = c(1, 2, 3, 4, 5, 1, 2),
                                       X1 = c(1, 4, 2, 3, 2, 1, 0),
                                       X2 = c(5, 4, 6, 3.28571428571429, 1, 4, 0),
                                       X1.dummy.var_GQ9KqD7YOf = structure(c(0L,0L, 1L, 0L, 0L, 0L, 0L),
                                                                           predictors.matching.dummy = "X1"),
                                       X2.dummy.var_GQ9KqD7YOf = structure(c(0L, 0L, 0L, 1L, 0L, 0L, 0L),
                                                                           predictors.matching.dummy = "X2")),
                                  row.names = c(1L, 2L, 3L, 4L, 5L, 7L, 8L), class = "data.frame")

factor.in.df <- data.frame(Y = 1:3, X1 = factor(1:3, labels = LETTERS[1:3]), X2 = 1:3)
missing.factor.in.df <- data.frame(Y = 1:3, X1 = factor(c(NA, 2:3), labels = LETTERS[2:3]), X2 = 1:3)
expected.missing.factor <- data.frame(Y = 1:3, X1 = factor(c(2, 2:3), labels = LETTERS[2:3]), X2 = 1:3,
                                      X1.dummy.var_GQ9KqD7YOf = c(1, 0, 0))
attr(expected.missing.factor[["X1.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- "X1"
df.with.text <- data.frame(Y = 1:2, X1 = 1:2, X3 = c(NA, LETTERS[1]),
                           stringsAsFactors = FALSE)
edge.case <- data.frame(Y = 1:5, X = 1:5)

expected.edge.with.missing <- structure(list(Y = 1:5, X = c(4.5, 4.5, 4.5, 4, 5),
                                             X.dummy.var_GQ9KqD7YOf = c(1L, 1L, 1L, 0L, 0L)),
                                        class = "data.frame", row.names = c(NA, -5L))
attr(expected.edge.with.missing[["X.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- "X"

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
    expect_equal(AddDummyVariablesForNAs(larger.case.with.missing.df, outcome.name = "Y"),
                 expected.larger.case)
    expect_equal(AddDummyVariablesForNAs(larger.case.with.missing.df, outcome.name = "Y", checks = TRUE),
                 expected.larger.case)
    # Check factors ok
    expect_identical(AddDummyVariablesForNAs(factor.in.df, outcome.name = "Y"), factor.in.df)
    expect_equal(AddDummyVariablesForNAs(missing.factor.in.df, outcome.name = "Y"),
                 expected.missing.factor)

    expect_error(AddDummyVariablesForNAs(df.with.text, outcome.name = "Y"),
                 "Unexpected class when using dummy variable adjustment.")
    # Check edge case with one predictor
    expect_identical(AddDummyVariablesForNAs(edge.case, outcome.name = "Y"),
                     edge.case)
    # Recode some to missing in predictor
    edge.case[1:3, 2] <- NA
    expect_identical(AddDummyVariablesForNAs(edge.case, outcome.name = "Y"),
                     expected.edge.with.missing)
})


test_that("DS-2986: Check aliased dummy variables reduced and mapping retained", {
    aliased.missing <- data.frame(Y = 1:2, X1 = c(NA, 2), X2 = c(NA, 3), X3 = c(1:2))
    exp.aliased.missing <- data.frame(Y = 1:2, X1 = c(2, 2), X2 = c(3, 3), X3 = 1:2,
                                      X1.dummy.var_GQ9KqD7YOf = c(1, 0))
    attr(exp.aliased.missing[["X1.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- paste0("X", 1:2)
    expect_equal(AddDummyVariablesForNAs(aliased.missing, outcome.name = "Y"),
                 exp.aliased.missing)
    multi.aliased.missing <- data.frame(Y = 1:2, X1 = c(NA, 2), X2 = c(NA, 3), X3 = c(1:2),
                                        X4 = c(1, NA), X5 = 3:4, X6 = c(2, NA))
    exp.multi.missing <- data.frame(Y = 1:2, X1 = c(2, 2), X2 = c(3, 3), X3 = 1:2,
                                    X4 = c(1, 1), X5 = c(3, 4), X6 = c(2, 2),
                                    X1.dummy.var_GQ9KqD7YOf = c(1, 0),
                                    X4.dummy.var_GQ9KqD7YOf = c(0, 1))
    attr(exp.multi.missing[["X1.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- paste0("X", 1:2)
    attr(exp.multi.missing[["X4.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- paste0("X", c(4, 6))
    expect_equal(AddDummyVariablesForNAs(multi.aliased.missing, outcome.name = "Y"),
                 exp.multi.missing)
    # Attributes retained when filtering
    multi.aliased.missing <- data.frame(Y = 1:3, X1 = c(NA, 2, NA), X2 = c(NA, 3, NA), X3 = c(1:2, NA),
                                        X4 = c(1, NA, NA), X5 = c(3:4, NA), X6 = c(2, NA, NA))
    exp.multi.missing <- data.frame(Y = 1:2, X1 = c(2, 2), X2 = c(3, 3), X3 = 1:2,
                                    X4 = c(1, 1), X5 = c(3, 4), X6 = c(2, 2),
                                    X1.dummy.var_GQ9KqD7YOf = c(1, 0),
                                    X4.dummy.var_GQ9KqD7YOf = c(0, 1))
    attr(exp.multi.missing[["X1.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- paste0("X", 1:2)
    attr(exp.multi.missing[["X4.dummy.var_GQ9KqD7YOf"]], "predictors.matching.dummy") <- paste0("X", c(4, 6))
    expect_equal(AddDummyVariablesForNAs(multi.aliased.missing, outcome.name = "Y"),
                 exp.multi.missing)
})
