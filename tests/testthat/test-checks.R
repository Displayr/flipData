context("Checks")


test_that("CheckForPositiveVariance",
{
     expect_error(CheckForPositiveVariance(data.frame(a = 1:10, b = 1)))
     expect_error(CheckForPositiveVariance(data.frame(a = 1:10, b = 1:10)), NA)
})

test_that("CheckCorrelationMatrix",
{
     expect_error(suppressWarnings(CheckCorrelationMatrix(cor(data.frame(a = 1:10, b = 1)))))
     expect_error(CheckCorrelationMatrix(cor(data.frame(a = 1:10, b = 1:10))), NA)
})

test_that("CheckForLinearDependence",
{
    # Raw data
    expect_error(CheckForLinearDependence(data.frame(a = 1:10, b = 1)), NA)
    expect_error(CheckForLinearDependence(data.frame(a = 1:10, b = 1:10)))
    expect_error(CheckForLinearDependence(data.frame(a = 1:10, b = c(1:5, 5, 5, 5, 5, 5), c = c(0,0,0,0,0:5))))
    # Correlations
    expect_error(CheckForLinearDependence(suppressWarnings(cor(data.frame(a = 1:10, b = 1)))))
    expect_error(CheckForLinearDependence(cor(data.frame(a = 1:10, b = 1:10))))
    expect_error(CheckForLinearDependence(cor(data.frame(a = 1:10, b = c(1:5, 5, 5, 5, 5, 5), c = c(0,0,0,0,0:5)))))
})

test_that("CheckForUniqueVariableNames",
{
     expect_error(CheckForUniqueVariableNames(y ~ x + y + y))
     expect_error(CheckForUniqueVariableNames(z$y ~ z$x + z$y + z$y))
     expect_error(CheckForUniqueVariableNames(y ~ x), NA)
     expect_error(CheckForUniqueVariableNames(z$y ~ z$x), NA)
})

data(hbatwithsplits, package = "flipExampleData")
#library(flipMultivariates)
test_that("CheckPredictionVariables",
{
    # Warning - not all levels are present in training data
    #expect_warning(z <- RandomForest(x3 ~ x1 + x2 + x6, data = hbatwithsplits, subset = !(x1 %in% "Less than 1 year")), "Some categories*")
    # Error - predicting based on fewer variables than used to fit model
    #expect_error(predict(z, newdata = hbatwithsplits[, !(names(hbatwithsplits) %in% "x2")]), "Attmpting to predict*")
    # Warning - more levels in prediction data than fitted
    #expect_error(predict(z, newdata = hbatwithsplits), "Prediction variable x1*")
    # No error - fewer levels in prediction data than fitted
    # TO DO
    # expect_error(****, NA)
})
