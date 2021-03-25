context("Stacking")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

common.labels <- c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                   "Diet Pepsi", "Pepsi Max", "None of these")

test_that("stacking", {
    StackData(findInstDirFile("Cola.sav"), common.labels = common.labels)
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
                          "variable name has been correctly specified."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             common.labels = common.labels,
                             variables.to.omit = "not-range"),
                   paste0("The start variable from the input range ",
                          "'not-range' could not be identified. The input ",
                          "range has been ignored. Ensure that the variable ",
                          "name is correctly specified."))
})
