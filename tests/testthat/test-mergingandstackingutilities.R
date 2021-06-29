test_that("variableType", {
    expect_equal(variableType(variable = 1:10), "Numeric")
    expect_equal(variableType(variable = LETTERS), "Text")
    categorical.var <- structure(c(1, 2, 1),
                                 labels = structure(1:2, .Names = c("A", "B")))
    expect_equal(variableType(variable = categorical.var), "Categorical")
    dates <- structure(c(18802, 18803, 18804), class = "Date")
    expect_equal(variableType(variable = dates), "Date")
    date.times <- structure(c(1624538040, 1624589160, 1624733100),
                            class = c("POSIXct", "POSIXt"), tzone = "UTC")
    expect_equal(variableType(variable = date.times), "Date/Time")
    duration <- structure(10:12, class = "difftime", units = "mins")
    expect_equal(variableType(variable = duration), "Duration")
})

test_that("isDateType", {
    expect_equal(isDateType(var.types = c("Date", "Date/Time", "Numeric")),
                 c(TRUE, TRUE, FALSE))
})

test_that("allIdentical", {
    expect_equal(allIdentical(x = 1:3), FALSE)
    expect_equal(allIdentical(x = c(1, 1, 1)), TRUE)
})

test_that("removeNA", {
    expect_equal(removeNA(x = c(NA, 1, 2, NA, 3)), c(1, 2 ,3))
})

test_that("splitByComma", {
    expect_equal(splitByComma(input.text = "Q1,Q2"), c("Q1", "Q2"))
    expect_equal(splitByComma(input.text = ",Q1,Q2, ,,Q3,"), c("Q1", "Q2", "Q3"))
    expect_equal(splitByComma(input.text = ",Q1(,2, 3,,), ,, Q2(3,4),,Q3,",
                              ignore.commas.in.parentheses = TRUE),
                 c("Q1(,2, 3,,)", "Q2(3,4)", "Q3"))
})

test_that("isIntegerValued", {
    expect_true(isIntegerValued(x = c(-1, 0, 1, 2, 3, NA, Inf)))
    expect_false(isIntegerValued(x = c(0, 1.1, NA)))
})

test_that("correctDataSetName", {
    expect_equal(correctDataSetName(data.set.name = NULL,
                                    default.data.set.name = "Merged data set.sav"),
                 "Merged data set.sav")
    expect_equal(correctDataSetName(data.set.name = "",
                                    default.data.set.name = "Merged data set.sav"),
                 "Merged data set.sav")
    expect_equal(correctDataSetName(" merged "), "merged.sav")
    expect_equal(suppressWarnings(correctDataSetName(data.set.name = "merged?")),
                 "merged.sav")
    expect_warning(correctDataSetName(data.set.name = "merged?"),
                   paste0("The input data set name 'merged?' contains ",
                          "invalid characters that have been removed."),
                   fixed = TRUE)
})

test_that("dataSetNameWithoutPath", {
    expect_equal(dataSetNameWithoutPath(data.set.name.or.path = "inst/testdata/Cola.sav"),
                 "Cola.sav")
})

test_that("throwVariableNotFoundError", {
    expect_error(throwVariableNotFoundError(var.name = "Q1", data.set.index = 2),
                 paste0("The input variable 'Q1' could not be found in ",
                        "input data set 2. Ensure that the variable has been ",
                        "correctly specified."))
})

test_that("uniqueName", {
    expect_equal(uniqueName(new.name = "Q2",
                            existing.names = c("Q1", "Q2", "Q3")), "Q21")
    expect_equal(uniqueName(new.name = "Q2",
                            existing.names = c("Q1", "Q2", "Q3"),
                            delimiter = "_"), "Q2_1")
})

test_that("parseVariableWildcardForMerging", {
    expect_equal(parseVariableWildcardForMerging(wildcard.text = c("Coke*"),
                                                 variable.names = c("Coca-cola",
                                                                    "Coke Zero",
                                                                    "Diet Coke",
                                                                    "Coke No Sugar"),
                                                 data.set.ind = 1,
                                                 error.if.not.found = FALSE),
                 c("Coke Zero", "Coke No Sugar"))

    expect_error(parseVariableWildcardForMerging(wildcard.text = c("Pepsi*"),
                                                 variable.names = c("Coca-cola",
                                                                    "Coke Zero",
                                                                    "Diet Coke",
                                                                    "Coke No Sugar"),
                                                 data.set.ind = 1,
                                                 error.if.not.found = TRUE),
                 paste0("No variables were found in data set 1 matching the ",
                        "wildcard input 'Pepsi*'."), fixed = TRUE)
})
