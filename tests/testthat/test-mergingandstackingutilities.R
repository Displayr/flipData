test_that("variableType", {
    expect_equal(variableType(1:10), "Numeric")
    expect_equal(variableType(LETTERS), "Text")
    categorical.var <- structure(c(1, 2, 1),
                                 labels = structure(1:2, .Names = c("A", "B")))
    expect_equal(variableType(categorical.var), "Categorical")
    dates <- structure(c(18802, 18803, 18804), class = "Date")
    expect_equal(variableType(dates), "Date")
    date.times <- structure(c(1624538040, 1624589160, 1624733100),
                            class = c("POSIXct", "POSIXt"), tzone = "UTC")
    expect_equal(variableType(date.times), "Date/Time")
})

test_that("isDateType", {
    expect_equal(isDateType(c("Date", "Date/Time", "Numeric")),
                 c(TRUE, TRUE, FALSE))
})

test_that("allIdentical", {
    expect_equal(allIdentical(1:3), FALSE)
    expect_equal(allIdentical(c(1, 1, 1)), TRUE)
})

test_that("removeNA", {
    expect_equal(removeNA(c(NA, 1, 2, NA, 3)), c(1, 2 ,3))
})

test_that("splitByComma", {
    expect_equal(splitByComma("Q1,Q2"), c("Q1", "Q2"))
    expect_equal(splitByComma(",Q1,Q2, ,,Q3,"), c("Q1", "Q2", "Q3"))
    expect_equal(splitByComma(",Q1(,2, 3,,), ,, Q2(3,4),,Q3,", ignore.commas.in.parentheses = TRUE),
                 c("Q1(,2, 3,,)", "Q2(3,4)", "Q3"))
})

test_that("isIntegerValued", {
    expect_true(isIntegerValued(c(-1, 0, 1, 2, 3, NA, Inf)))
    expect_false(isIntegerValued(c(0, 1.1, NA)))
})

test_that("correctDataSetName", {
    expect_equal(correctDataSetName(NULL, "Merged data set.sav"),
                 "Merged data set.sav")
    expect_equal(correctDataSetName("", "Merged data set.sav"),
                 "Merged data set.sav")
    expect_equal(correctDataSetName(" merged "), "merged.sav")
    expect_equal(suppressWarnings(correctDataSetName("merged?")), "merged.sav")
    expect_warning(correctDataSetName("merged?"),
                   paste0("The input data set name 'merged?' contains ",
                          "invalid characters that have been removed."),
                   fixed = TRUE)
})

test_that("dataSetNameWithoutPath", {
    expect_equal(dataSetNameWithoutPath("inst/testdata/Cola.sav"), "Cola.sav")
})

test_that("throwVariableNotFoundError", {
    expect_error(throwVariableNotFoundError("Q1", 2),
                 paste0("The input variable 'Q1' could not be found in the ",
                        "input data set 2. Ensure that the variable has been ",
                        "correctly specified."))
})

test_that("uniqueName", {
    expect_equal(uniqueName("Q2", c("Q1", "Q2", "Q3")), "Q21")
    expect_equal(uniqueName("Q2", c("Q1", "Q2", "Q3"), delimiter = "_"), "Q2_1")
})
