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
test_that("CheckPredictionVariables",
{
    data <- GetData(x3 ~ x1 + x2 + x6, hbatwithsplits, auxiliary.data = NULL)
    z <- list(model = data, outcome.name = "x3", subset = !(hbatwithsplits$x1 %in% "Less than 1 year")) # remove a level

    # Predicting based on fewer variables than used to fit model
    expect_error(CheckPredictionVariables(z, newdata = hbatwithsplits[, !(names(hbatwithsplits) %in% "x2")]), "Attempting to predict*")

    # More levels in prediction data than fitted
    newdata <- hbatwithsplits
    attr(newdata$x1, "Label") <- "something"
    expect_warning(checked <- CheckPredictionVariables(z, newdata = newdata),
                   "Prediction variable x1 contains categories (Less than 1 year) that were not used for training. 32 instances are affected.",
                   fixed = TRUE)
    expect_equal(attr(checked$x1, "Label"), "something")

    # Prediction levels reset to those used for fitting
    expect_equal(length(levels(CheckPredictionVariables(z, newdata = droplevels(hbatwithsplits[1, ]))$x1)), 2)

    # Amend levels of a factor
    single.data <- data[1, , drop = FALSE]

    # Fewer levels in prediction data than fitted
    single.data[, "x1"] <- as.factor("Over 5 years")
    amended <- CheckPredictionVariables(z, newdata = single.data)
    expect_equal(levels(amended$x1), levels(checked$x1))
    # Check levels have not been reordered
    expect_equal(as.character(amended$x1), "Over 5 years")

    # Input string of factor level
    single.data[, "x1"] <- as.character("Over 5 years")
    amended <- CheckPredictionVariables(z, newdata = single.data)
    expect_equal(levels(amended$x1), levels(checked$x1))

    # Input string which is not a level
    single.data[, "x1"] <- as.character("Not a level")
    expect_warning(CheckPredictionVariables(z, newdata = single.data),
                   "Prediction variable x1 contains categories (Not a level) that were not used for training. 1 instances are affected.",
                   fixed = TRUE)

})

