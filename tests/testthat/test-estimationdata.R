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


dat2 <- structure(list(Country = structure(c(1L, 4L, 2L, 3L), class = "factor",
                .Label = c("Australia","Denmark", "Fiji", "France"),
                questiontype = "PickOne", name = "Country", label = "Country", question = "Country"),
                  A = structure(c(1L, 2L, 3L, NA), class = "factor", .Label = c("1",
                 "2", "3"), questiontype = "PickOne", name = "A", label = "A", question = "A")),
                class = "data.frame", row.names = c(NA, -4L))
filt <- c(FALSE, TRUE, TRUE, TRUE)
EstimationData(~Country + A, data = dat2, subset = filt)




