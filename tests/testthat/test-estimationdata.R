library(testthat)
context("Estimation data")


context("LDA")

data(hbatwithsplits, package = "flipExampleData")
hair <- hbatwithsplits
library(flipMultivariates)
hair1  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)
hair1$x1 <- hair$x1
hair1$split60 <- hair$split60
hair1$id <- hair$id
LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, method = "moment", data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE)



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


test_that("Removing unused factors prior to regression", {

    data(phone, package = "flipExampleData")
    levs <- attr(phone$q3, "value.labels")

    z <- phone$q3
    z[is.na(z)] <- 100
    z <- as.numeric(z)
    z <- factor(z)

    lv <- c("-9", "0", names(levs[7:1]), "100")
    levels(z) <- lv
    z[z == "100"] <- NA
    phone$q3 <- z

    #expect_error(flipRegression::Regression(q3 ~ q2, data = phone))
    expect_error(suppressWarnings(flipRegression::Regression(q3 ~ q2, data = phone, missing = "Multiple imputation")), NA)
})

test_that("Duplicate variables", {

    data(phone, package = "flipExampleData")
    expect_error(flipRegression::Regression(q2 ~ q2 + q3, data = phone))
})





