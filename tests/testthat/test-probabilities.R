context("Probabilities")

test_that("Probabilities", {
    # See test-checks.R for tests about Probabilities(object) which uses the model
    # Test newdata argument
    bad.newdata <- list(1:10, list(), TRUE, 1 + 1i, "")
    dummy.object <- list()
    for (newdat in bad.newdata)
       expect_error(Probabilities(object = dummy.object, newdata = newdat),
                    "newdata must be a data.frame")
    expect_error(Probabilities(object = dummy.object, newdata = data.frame()),
                 "Need at least one observation in the newdata argument")
})
