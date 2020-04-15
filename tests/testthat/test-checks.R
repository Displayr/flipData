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
    attr(z$model$x1, "label") <- "something"
    attr(newdata$x1, "label") <- "something"
    expect_warning(checked <- CheckPredictionVariables(z, newdata = newdata),
                   paste0("The prediction variable ", sQuote("something"), " contained the category ",
                          "(Less than 1 year) that was not used in the training data. It is not possible to predict ",
                          "outcomes in these cases and are coded as missing as a result. 32 instances were affected. ",
                          "If non-missing predictions are required, consider merging categories if merging categories ",
                          "is applicable for this variable"),
                   fixed = TRUE)
    expect_equal(attr(checked$x1, "label"), "something")

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
                   paste0("The prediction variable ", sQuote("something"), " contained the category ",
                          "(Not a level) that was not used in the training data. It is not possible to predict ",
                          "outcomes in these cases and are coded as missing as a result. 1 instance was affected. ",
                          "If non-missing predictions are required, consider merging categories if merging categories ",
                          "is applicable for this variable"),
                   fixed = TRUE)

    # Check fallback to variable name if label not available
    attr(z$model$x1, "label") <- attr(z$model$x1, "question") <- NULL
    expect_warning(CheckPredictionVariables(z, newdata = single.data),
                   paste0("The prediction variable ", sQuote("x1"), " contained the category ",
                          "(Not a level) that was not used in the training data. It is not possible to predict ",
                          "outcomes in these cases and are coded as missing as a result. 1 instance was affected. ",
                          "If non-missing predictions are required, consider merging categories if merging categories ",
                          "is applicable for this variable"),
                   fixed = TRUE)
})

test_that("DS-28704 Automated outlier removal scenario catches missing levels", {
    data <- GetData(x3 ~ x1 + x2 + x6, hbatwithsplits, auxiliary.data = NULL)
    z <- list(model = data, outcome.name = "x3", subset = !(hbatwithsplits$x1 %in% "Less than 1 year"))
    # Make scenario where outliers have removed all instances of a level in a factor
    z$estimation.data <- z$model[z$subset, ]
    z$estimation.data$x1[1] <- "Less than 1 year"
    z$non.outlier.data <- z$estimation.data$x1 != "Over 5 years"
    n.affected <- sum(!z$non.outlier.data)
    z$estimation.data$non.outlier.data_GQ9KqD7YOf <- z$non.outlier.data
    class(z) <- "Regression"
    attr(z$model$x1, "label") <- "Time"
    expect_warning(CheckPredictionVariables(z, newdata = hbatwithsplits),
                   paste0("The prediction variable ", sQuote("Time"), " contained the category ",
                          "(Over 5 years) that was not used in the training data since the automated outlier ",
                          "removal identified those observations as outliers and removed them. ",
                          "It is not possible to predict outcomes in these cases and are coded as missing as a result. ",
                          n.affected, " instances were affected. If non-missing predictions are required, consider merging ",
                          "categories if merging categories is applicable for this variable"),
                   fixed = TRUE)
    # Test situation where two levels are not in training data.
    z$estimation.data$x1[1] <- "1 to 5 years"
    expect_warning(CheckPredictionVariables(z, newdata = hbatwithsplits),
                   paste0("The prediction variable ", sQuote("Time"), " contained categories ",
                          "(Less than 1 year, Over 5 years) that were not used in the training data. ",
                          "It is not possible to predict outcomes in these cases and are coded as missing as a result. ",
                          "32 and 33 instances respectively were affected. If non-missing predictions are required, consider merging ",
                          "categories if merging categories is applicable for this variable"),
                   fixed = TRUE)
})

