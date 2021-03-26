context("Stacking")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

common.labels <- c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                   "Diet Pepsi", "Pepsi Max", "None of these")

test_that("stacking", {
    result <- StackData(findInstDirFile("Cola.sav"), common.labels = common.labels)

    # Check stacked values!
})

test_that("omitted variables", {
    result <- StackData(findInstDirFile("Cola.sav"),
                        common.labels = common.labels,
                        variables.to.omit = "Q2, Q4_A, Q9_*,-Progress, GZfrequentCola-,Q29-Q31")

    omitted.variables <- c("URLID", "Type", "Progress", "Q2", "Q4_A", "Q9_A", "Q9_", "Q9_E",
                           "Q9_F", "Q29", "Q29_G_O", "Q30_A", "Q30_B", "Q30_C", "Q30_D",
                           "Q30_E", "Q30_F", "Q30_G", "Q30_H", "Q31", "GZfrequentCola",
                           "GZletters", "GZloopTextQ5", "GZrandomCola", "GZstatus", "GZtopPrefs")

    expect_equal(result$omitted.variables, omitted.variables)
    expect_equal(result$omitted.stacked.variables, "Q9_")
    expect_true(!any(omitted.variables %in%
                         result$stacked.data.set.metadata$variable.names))

    # variables.to.omit with multiple entries
    result <- StackData(findInstDirFile("Cola.sav"),
                        common.labels = common.labels,
                        variables.to.omit = c("Q2, Q4_A", "Q9_*", "-Progress,
                                              GZfrequentCola-","Q29-Q31"))
    expect_equal(result$omitted.variables, omitted.variables)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             variables.to.omit = "not_a_variable"),
                   paste0("The input varible name 'not_a_variable' ",
                          "could not be identified and has been ignored."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             variables.to.omit = "not_*_variable"),
                   paste0("No matches were found for the wildcard variable ",
                          "name 'not_*_variable'. Ensure that the wildcard ",
                          "variable name has been correctly specified."),
                   fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             variables.to.omit = "not-range"),
                   paste0("The start variable from the input range ",
                          "'not-range' could not be identified. The input ",
                          "range has been ignored. Ensure that the variable ",
                          "name is correctly specified."))
})

test_that("manual stacking by variables", {
    result <- StackData(findInstDirFile("Cola.sav"),
                        common.labels = common.labels,
                        specify.by = "Variable",
                        manual.stacking = c("Q6_*, NA", "Q9_A, Q9_B, Q9_C-Q9_F"))

    md <- result$stacked.data.set.metadata
    expect_true("Q6_" %in% md$variable.names)
    expect_true("Q9_" %in% md$variable.names)
    expect_true(md$is.stacked.variable["Q6_"])
    expect_true(md$is.stacked.variable["Q9_"])
    expect_equal(md$stacking.input.variable.names[["Q6_"]],
                 c("Q6_A", "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F", NA))
    expect_equal(md$stacking.input.variable.names[["Q9_"]],
                 c("Q9_A", "Q9_B", "Q9_C", "Q9_D", "Q9_E", "Q9_F", NA))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             specify.by = "Variable",
                             manual.stacking = "Q6_A, Q6_B, not_a_variable"),
                   paste0("The manual stacking input varible name ",
                          "'not_a_variable' could not be identified. The ",
                          "manual stacking input 'Q6_A, Q6_B, not_a_variable'",
                          "has been ignored."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             specify.by = "Variable",
                             manual.stacking = c("Q6_*", "Q6_A-Q6_F")),
                   paste0("The manual stacking input 'Q6_A, Q6_*' has been ",
                          "ignored as it contains duplicate entries for ",
                          "'Q6_A'."), fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             specify.by = "Variable",
                             manual.stacking = "Q6_A, Q6_*"),
                   paste0("The manual stacking input 'Q6_A-Q6_F' has been ",
                          "ignored as it contains variable(s) that overlap ",
                          "with another manual stacking input 'Q6_*'."),
                   fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             specify.by = "Variable",
                             manual.stacking = "Q6_A,not_*_variable"),
                   paste0("No matches were found for the manual stacking ",
                          "input wildcard name 'not_*_variable'. Ensure that ",
                          "the wildcard variable name has been correctly ",
                          "specified. The manual stacking input ",
                          "'Q6_A,not_*_variable' has been ignored."),
                   fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             specify.by = "Variable",
                             manual.stacking = "Q6_A-not_a_variable"),
                   paste0("The end variable from the manual stacking input ",
                          "range 'Q6_A-not_a_variable' could not be ",
                          "identified. The manual stacking input ",
                          "'Q6_A-not_a_variable' has been ignored. ",
                          "Ensure that the variable name is correctly ",
                          "specified."))
})
