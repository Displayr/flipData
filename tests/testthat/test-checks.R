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
    # newdata argument is provided and non-empty
    expected.err <- paste0(sQuote("newdata"), " argument must be a data.frame ",
                           "with at least one observation.")
    bad.newdata <- list(1:10, TRUE, 1+1i, "foo", list())
    for (newdat in bad.newdata)
        expect_error(CheckPredictionVariables(z, newdata = newdat), expected.err, fixed = TRUE)
    expect_error(CheckPredictionVariables(z, newdata = NULL), expected.err, fixed = TRUE)
    expect_error(CheckPredictionVariables(z, newdata = data.frame()), expected.err, fixed = TRUE)

    # Predicting based on fewer variables than used to fit model
    test.formula <- x3 ~ x1 + x2 + x6
    data <- GetData(test.formula, hbatwithsplits, auxiliary.data = NULL)
    z <- structure(
        list(formula = test.formula,
             model = data,
             outcome.name = "x3",
             subset = !(hbatwithsplits$x1 %in% "Less than 1 year") # remove a level
        ),
        class = "Regression"
    )

    # Predicting based on fewer variables than used to fit model
    expected.error <- paste0("Attempting to predict based on fewer variables than ",
                             "those used to train the model.")
    smaller.newdat <- hbatwithsplits[, !(names(hbatwithsplits) %in% "x2")]
    expect_error(CheckPredictionVariables(z, newdata = smaller.newdat),
                 expected.error, fixed = TRUE)

    # More levels in prediction data than fitted
    newdata <- hbatwithsplits
    attr(z$model$x1, "label") <- "something"
    attr(newdata$x1, "label") <- "something"
    expected.warn <- paste0(
        "The prediction variable ", sQuote("something"), " contained the category ",
        "(", sQuote("Less than 1 year"), ") that was not used in the training data. ",
        "It is not possible to predict outcomes in these cases and they are coded as ",
        "missing as a result. 32 instances were affected. If non-missing predictions ",
        "are required, consider merging categories if merging categories is ",
        "applicable for this variable."
    )
    expect_warning(checked <- CheckPredictionVariables(z, newdata = newdata),
                   expected.warn, fixed = TRUE)
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

    # Expect error if object not of class CART, Regression or MachineLearning
    valid.classes <- c("CART", "MachineLearning", "Regression")
    expected.err <- capture_error(
        throwErrorUnsupportedPredictionClass(valid.classes)
    )[["message"]]
    expect_error(CheckPredictionVariables(unclass(z), newdata = single.data),
                 expected.err, fixed = TRUE)

    # Input string of factor level
    single.data[, "x1"] <- as.character("Over 5 years")
    amended <- CheckPredictionVariables(z, newdata = single.data)
    expect_equal(levels(amended$x1), levels(checked$x1))

    # Input string which is not a level
    single.data[, "x1"] <- as.character("Not a level")
    expect_warning(CheckPredictionVariables(z, newdata = single.data),
                   paste0("The prediction variable ", sQuote("something"), " contained the category ",
                          "(", sQuote("Not a level"), ") that was not used in the training data. It is not possible to predict ",
                          "outcomes in these cases and they are coded as missing as a result. 1 instance was affected. ",
                          "If non-missing predictions are required, consider merging categories if merging categories ",
                          "is applicable for this variable."),
                   fixed = TRUE)

    # Check fallback to variable name if label not available
    attr(z$model$x1, "label") <- attr(z$model$x1, "question") <- NULL
    expect_warning(CheckPredictionVariables(z, newdata = single.data),
                   paste0("The prediction variable ", sQuote("x1"), " contained the category ",
                          "(", sQuote("Not a level"), ") that was not used in the training data. It is not possible to predict ",
                          "outcomes in these cases and they are coded as missing as a result. 1 instance was affected. ",
                          "If non-missing predictions are required, consider merging categories if merging categories ",
                          "is applicable for this variable."),
                   fixed = TRUE)
})


test_that("DS-2704 Automated outlier removal scenario catches missing levels", {
    test.formula <- x3 ~ x1 + x2 + x6
    data <- GetData(test.formula, hbatwithsplits, auxiliary.data = NULL)
    z <- list(formula = test.formula, model = data, outcome.name = "x3", subset = !(hbatwithsplits$x1 %in% "Less than 1 year"))
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
                          "(", sQuote("Over 5 years"), ") that was not used in the training data since the automated outlier ",
                          "removal identified those observations as outliers and removed them. ",
                          "It is not possible to predict outcomes in these cases and they are coded as missing as a result. ",
                          n.affected, " instances were affected. If non-missing predictions are required, consider merging ",
                          "categories if merging categories is applicable for this variable."),
                   fixed = TRUE)
    # Test situation where two levels are not in training data.
    z$estimation.data$x1[1] <- "1 to 5 years"
    expect_warning(CheckPredictionVariables(z, newdata = hbatwithsplits),
                   paste0("The prediction variable ", sQuote("Time"), " contained categories ",
                          "(", sQuote("Less than 1 year"), " and ", sQuote("Over 5 years"), ") ",
                          "that were not used in the training data. ",
                          "It is not possible to predict outcomes in these cases and they are coded as missing ",
                          "as a result. 32 and 33 instances respectively were affected. If non-missing predictions ",
                          "are required, consider merging categories if merging categories is applicable ",
                          "for this variable."),
                   fixed = TRUE)
    # Test situation where three levels are not in training data.
    newdata <- hbatwithsplits
    z$estimation.data$x1[1] <- "1 to 5 years"
    levels(newdata$x1) <- c(levels(newdata$x1), "Unknown")
    newdata$x1[4] <- "Unknown"
    expect_warning(CheckPredictionVariables(z, newdata = newdata),
                   paste0("The prediction variable ", sQuote("Time"), " contained categories ",
                          "(", sQuote("Less than 1 year"), ", ", sQuote("Over 5 years"), " and ", sQuote("Unknown"),
                          ") that were not used in the training data. It is not possible to predict outcomes in these ",
                          "cases and they are coded as missing as a result. 31, 33 and 1 instances respectively were ",
                          "affected. If non-missing predictions are required, consider merging categories if merging ",
                          "categories is applicable for this variable"),
                   fixed = TRUE)
})

test_that("DS-3488 Check dummy variable adjustment handled with and without outlier removal", {
    missing.all.predictors <- data.frame(Y = c(1:20, 100),
                                         X1 = c(NA, 1:5, NA, 7:20),
                                         X2 = c(NA, runif(20)))
    dummy.adj.model <- AddDummyVariablesForNAs(missing.all.predictors, "Y", checks = FALSE)
    estimation.data <- EstimationData(Y ~ X1 + X2, data = missing.all.predictors,
                                      missing = "Dummy variable adjustment")$estimation.data
    output <- structure(list(formula = Y ~ X1 + X2 + X1.dummy.var_GQ9KqD7YOf,
                             estimation.data = estimation.data,
                             model = dummy.adj.model,
                             subset = TRUE,
                             outcome.name = "Y"),
                   class = "Regression")
    expected.output <- dummy.adj.model[, c("X1", "X2", "X1.dummy.var_GQ9KqD7YOf")]
    expect_equal(CheckPredictionVariables(output, newdata = dummy.adj.model),
                 expected.output)
    output[["non.outlier.data"]] <- rep(c(TRUE, FALSE), c(19, 1))

    expect_equal(CheckPredictionVariables(output, newdata = dummy.adj.model),
                 expected.output)
    # Check method still works when formula not available
    output[["formula"]] <- NULL
    output[["outcome.name"]] <- "Y"
    output[["model"]] <- missing.all.predictors
    expected.output <- missing.all.predictors[-1, -1]
    expect_equal(CheckPredictionVariables(output, newdata = dummy.adj.model),
                 dummy.adj.model[, c("X1", "X2")])
    # Check estimation data is used and only uses the correct variables after Importance analysis
    input.ed <- data.frame(Response = 1, w = 1, y = 1, z = 1, x.dummy.var_GQ9KqD7YOf = 1,
                           non.outlier.data_GQ9KqD7YOf = TRUE)
    new.data <- input.ed[2:5]
    object <- structure(list(formula = Response ~ w + x + y + z + x.dummy.var_GQ9KqD7YOf + z.dummy.var_GQ9KqD7YOf,
                             missing =  "Dummy variable adjustment",
                             model = data.frame(Response = 1, w = 1, y = 1, z = 1,
                                                x.dummy.var_GQ9KqD7YOf = 1,
                                                z.dummy.var_GQ9KqD7YOf = 1),
                             importance.type = "Shapley Regression",
                             estimation.data = input.ed),
                        class = "Regression")
    expect_equal(CheckPredictionVariables(object, input.ed), new.data)
})
