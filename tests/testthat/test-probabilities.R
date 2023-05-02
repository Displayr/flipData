context("Probabilities")

test_that("Probabilities", {
    bad.inputs <- list(list(), TRUE, 1, 1.0, "foo")
    valid.classes <- c("CART", "MachineLearning", "MachineLearningEnsemble", "Regression")
    expected.err <- capture_error(throwErrorUnsupportedPredictionClass(valid.classes))[["message"]]
    for (bad.input in bad.inputs) {
        expect_error(validateProbabilityArguments(bad.input, NULL), expected.err)
        expect_error(Probabilities(bad.input, NULL), expected.err)
    }
    # Data.frame empty
    invalid.input <- lapply(valid.classes, function(x) structure(list(model = data.frame()), class = x))
    expected.err <- paste0(sQuote("newdata"), " must be a data.frame with at least one observation")
    for (input in invalid.input) {
        expect_error(validateProbabilityArguments(input, NULL),
                     expected.err)
        expect_error(Probabilities(input, NULL),
                     expected.err)
    }
    invalid.input <- lapply(invalid.input, function(x) {
        x[["model"]] <- matrix(1, ncol = 1)
        x
    })
    for (input in invalid.input) {
        expect_error(validateProbabilityArguments(input, NULL),
                     expected.err)
        expect_error(Probabilities(input, NULL),
                     expected.err)
    }
    # Valid inputs
    valid.input <- lapply(valid.classes, function(x) structure(list(model = data.frame(x = 1)), class = x))
    valid.input.but.no.method.err <- paste0("object not supported")
    for (input in valid.input) {
        expect_silent(validateProbabilityArguments(input, NULL))
        expect_silent(validateProbabilityArguments(input, data.frame(x = 1)))
        # Default method will error as the methods are provided in flipTrees,
        # flipRegression and flipMultivariates
        expect_error(Probabilities(input, NULL), valid.input.but.no.method.err)
        expect_error(Probabilities(input, data.frame(x = 1)), valid.input.but.no.method.err)
    }
})
