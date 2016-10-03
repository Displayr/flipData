library(testthat)
context("Estimation data")

data(hbatwithsplits, package = "flipExampleData")
hair <- hbatwithsplits
library(flipMultivariates)
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

    expect_error(suppressWarnings(flipRegression::Regression(q3 ~ q2, data = phone, missing = "Multiple imputation")))
    expect_error(suppressWarnings(flipRegression::Regression(q2 ~ q3, data = phone, missing = "Multiple imputation")), NA)
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


test_that(paste("Grand mean"),
{
    type  = "Linear"
    library(flipRegression)
    z = suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank[, c("Overall", "Fees", "Interest","Phone", "Branch", "Online", "ATM")], type = type, subset = sb, weights = wgt))
    subset <- sb & wgt > 0 & !is.na(sb) & !is.na(wgt)
    subset <- subset & complete.cases(cbind(sb, wgt, bank[, c("Overall", "Fees", "Interest","Phone", "Branch", "Online", "ATM")]))
    y <- bank$Overall[subset]
    w <- wgt[subset]
    mn <- sum(y * w) / sum(w)
    expect_equal(mn, GrandMean(z))
})



