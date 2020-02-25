library(testthat)
context("Estimation data")

data(hbatwithsplits, package = "flipExampleData")
hair <- hbatwithsplits
hair1  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)
hair1$x1 <- hair$x1
hair1$split60 <- hair$split60
hair1$id <- hair$id


test_that("Single vs multiple imputation",
{
    data("bank", package = "flipExampleData")
    est <- EstimationData(Overall ~ Fees + Branch, bank, missing = "Imputation (replace missing values with estimates)")
    expect_is(est$estimation.data, "data.frame")
    est <- EstimationData(Overall ~ Fees + Branch, bank, missing = "Multiple imputation", m = 10)
    expect_is(est$estimation.data, "list")
    expect_equal(length(est$estimation.data), 10)
})

test_that("Checking that label is retained",
{
    data("bank", package = "flipExampleData")
    attr(bank$Overall, "label") <- "Overall satisfaction"

    est <- EstimationData(Overall ~ Fees + Branch, bank, missing = "Imputation (replace missing values with estimates)")
    expect_is(est$estimation.data, "data.frame")
    est <- EstimationData(Overall ~ Fees + Branch, bank, missing = "Multiple imputation", m = 10)
    expect_is(est$estimation.data, "list")
    expect_equal(length(est$estimation.data), 10)
})


test_that("Duplicate variables", {

    data(phone, package = "flipExampleData")
    expect_error(flipRegression::Regression(q2 ~ q2 + q3, data = phone))
})


zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank, package = "flipExampleData")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
wgt[is.na(wgt)] = 0
attr(wgt, "label") <- "ID"
attr(bank$Overall, "label") <- "Overall satisfaction"
attr(bank$Fees, "label") <- "Fees paid"
attr(bank$Online, "label") <- "Online banking"


test_that("DS-2626",
{
    dat2 <- structure(list(Country = structure(c(1L, 4L, 2L, 3L), class = "factor",
                .Label = c("Australia","Denmark", "Fiji", "France"),
                questiontype = "PickOne", name = "Country", label = "Country", question = "Country"),
                  A = structure(c(1L, 2L, 3L, NA), class = "factor", .Label = c("1",
                 "2", "3"), questiontype = "PickOne", name = "A", label = "A", question = "A")),
                class = "data.frame", row.names = c(NA, -4L))
    filt <- c(FALSE, TRUE, TRUE, TRUE)
    expect_warning(EstimationData(~Country + A, data = dat2, subset = filt),
            "Some categories do not appear in the data: 'Country (Country): Australia', 'Country (Country): Fiji', 'A (A): 1'", fixed = TRUE)
})

missing.level.test <- data.frame(Y = factor(c(1, 2, 2, 3, 3), labels = LETTERS[1:3]),
                                 X1 = c(NA, 1, NA, 3, 4),
                                 X2 = c(NA, 1, 2, 3, 4),
                                 X3 = c(NA, 3, 2, 1, 4))
expected.dummy.missing.level <- data.frame(Y = factor(c(2, 2, 3, 3), labels = LETTERS[2:3]),
                                           X1 = c(1, 0, 3, 4),
                                           X2 = c(1, 2, 3, 4),
                                           X3 = c(3, 2, 1, 4),
                                           X1.dummy.var_GQ9KqD7YOf = c(0, 1, 0, 0),
                                           row.names = 2:5)
no.level.test <- data.frame(Y  = c(1, 2, 2, 3, 3),
                            X1 = c(NA, 1, NA, 3, 4),
                            X2 = c(NA, 1, 2, 3, 4),
                            X3 = c(NA, 3, 2, 1, 4))


test_that("Dummy variable adjustment", {
    expect_warning(missing.level.output <- EstimationData(Y ~ X1 + X2 + X3, data = missing.level.test,
                                                          missing = "Dummy variable adjustment")$estimation.data,
                   "Some categories do not appear in the data: 'Y: A'")
    expect_equal(missing.level.output, expected.dummy.missing.level)
    expect_warning(EstimationData(Y ~ X1 + X2, data = no.level.test), NA)
    expect_error(EstimationData(Y ~ X1 + X2, no.level.test[1:4, ]),
                 "There are fewer observations (2) than there are variables (3)", fixed = TRUE)
})


