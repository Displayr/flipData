context("Stacking")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

common.labels <- c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                   "Diet Pepsi", "Pepsi Max", "None of these")

test_that("no stacking", {
    result <- StackData(findInstDirFile("Cola.sav"),
                        stack.with.common.labels = "Disabled")
    expect_false(any(result$stacked.data.set.metadata$is.stacked.variable))
    expect_equal(result$stacked.data.set.metadata$n.variables, 198)
})

test_that("common label stacking", {
    result.auto <- StackData(findInstDirFile("Cola.sav"))
    result <- StackData(findInstDirFile("Cola.sav"), common.labels = common.labels,
                        stack.with.common.labels = "Using manually input common labels",
                        include.stacked.data.set.in.output = TRUE)
    expect_true(all(c("Q1_", "Q5_5_", "Q5_7_") %in%
                        result.auto$stacked.data.set.metadata$variable.names))
    expect_true(all(c("Q1.", "Q5. [feminine]", "Q5. [health-conscious]") %in%
                        result.auto$stacked.data.set.metadata$variable.labels))
    expect_true(setequal(result.auto$stacked.data.set.metadata$variable.names,
                         result$stacked.data.set.metadata$variable.names))
    input <- readDataSets(findInstDirFile("Cola.sav"))[[1]]
    expect_equal(c(result$stacked.data.set$Q1_),
                 c(rbind(input$Q1_F, input$Q1_E, input$Q1_D, input$Q1_C,
                         input$Q1_A, input$Q1_B, rep(NA, nrow(input)))))
    expect_equal(as.numeric(result$stacked.data.set$Q2),
                 as.numeric(rep(input$Q2, each = 7)))
    expect_equal(as.numeric(result$stacked.data.set$original_case), rep(1:327, each = 7))
    expect_equal(as.numeric(result$stacked.data.set$observation), rep(1:7, 327))

    result <- StackData(findInstDirFile("Cola.sav"),
                        stack.with.common.labels = "Using a set of variables to stack as reference",
                        common.labels.variables = "Q5_5_*",
                        include.stacked.data.set.in.output = TRUE)
    expect_true(setequal(result.auto$stacked.data.set.metadata$variable.names,
                         result$stacked.data.set.metadata$variable.names))

    result <- StackData(findInstDirFile("Cola.sav"),
                        include.original.case.variable = FALSE,
                        include.observation.variable = FALSE)
    expect_false("original_case" %in% result$stacked.data.set.metadata$variable.names)
    expect_false("observation" %in% result$stacked.data.set.metadata$variable.names)
})

test_that("omitted variables", {
    result <- StackData(findInstDirFile("Cola.sav"),
                        variables.to.omit = "Q2, Q4_A, Q9_*,-Progress, GZfrequentCola-,Q29-Q31")

    omitted.variables <- c("URLID", "Type", "Progress", "Q2", "Q4_A", "Q9_A", "Q9_B",
                           "Q9_C", "Q9_D", "Q9_E", "Q9_F", "Q29", "Q29_G_O", "Q30_A", "Q30_B",
                           "Q30_C", "Q30_D", "Q30_E", "Q30_F", "Q30_G", "Q30_H", "Q31",
                           "GZfrequentCola", "GZletters", "GZloopTextQ5", "GZrandomCola",
                           "GZstatus", "GZtopPrefs")

    expect_equal(result$omitted.variables, omitted.variables)
    expect_true(!any(omitted.variables %in%
                         result$stacked.data.set.metadata$variable.names))

    result <- StackData(findInstDirFile("Cola.sav"),
                        variables.to.omit = "Q2, Q4_A, Q9_,-Progress, GZfrequentCola-,Q29-Q31")

    expect_equal(result$omitted.stacked.variables, "Q9_")
    expect_true("Q9_" %in% result$omitted.variables)
    expect_false("Q9_" %in% result$stacked.data.set.metadata$variable.names)

    # variables.to.omit with multiple entries
    result <- StackData(findInstDirFile("Cola.sav"),
                        variables.to.omit = c("Q2, Q4_A", "Q9_*", "-Progress,
                                              GZfrequentCola-","Q29-Q31"))
    expect_equal(result$omitted.variables, omitted.variables)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             variables.to.omit = "not_a_variable"),
                   paste0("The omitted variable input varible name 'not_a_variable' ",
                          "could not be identified. This input has been ignored."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             variables.to.omit = "not_*_variable"),
                   paste0("No matches were found for the omitted variable ",
                          "input wildcard name 'not_*_variable'. Ensure that ",
                          "the wildcard variable name has been correctly ",
                          "specified. This input has been ignored."),
                   fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             variables.to.omit = "not-range"),
                   paste0("The start variable from the omitted variable ",
                          "input range 'not-range' could not be identified. ",
                          "The input range has been ignored. Ensure that the ",
                          "variable name is correctly specified."))
})

test_that("manual stacking by variables", {
    result <- StackData(findInstDirFile("Cola.sav"),
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
                             specify.by = "Variable",
                             manual.stacking = "Q6_A, Q6_B, not_a_variable"),
                   paste0("The manual stacking input varible name ",
                          "'not_a_variable' could not be identified. The ",
                          "manual stacking input 'Q6_A, Q6_B, not_a_variable' ",
                          "has been ignored."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Variable",
                             manual.stacking = c("Q6_*", "Q6_A-Q6_F")),
                   paste0("The manual stacking input 'Q6_A-Q6_F' has been ",
                          "ignored as it contains variable(s) that overlap ",
                          "with another manual stacking input 'Q6_*'."),
                   fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Variable",
                             manual.stacking = "Q6_A, Q6_*"),
                   paste0("The manual stacking input 'Q6_A, Q6_*' has been ",
                          "ignored as it contains duplicate entries for 'Q6_A'"),
                   fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Variable",
                             manual.stacking = c("LastResp,Q3")),
                   paste0("The manual stacking input 'LastResp,Q3' has been ",
                          "ignored as it contains variables with mismatching ",
                          "types or value attributes."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Variable",
                             manual.stacking = "Q6_A,not_*_variable"),
                   paste0("No matches were found for the manual stacking ",
                          "input wildcard name 'not_*_variable'. Ensure that ",
                          "the wildcard variable name has been correctly ",
                          "specified. The manual stacking input ",
                          "'Q6_A,not_*_variable' has been ignored."),
                   fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Variable",
                             manual.stacking = "Q6_A-not_a_variable"),
                   paste0("The end variable from the manual stacking input ",
                          "range 'Q6_A-not_a_variable' could not be ",
                          "identified. The manual stacking input ",
                          "'Q6_A-not_a_variable' has been ignored. ",
                          "Ensure that the variable name is correctly ",
                          "specified."))
})

test_that("manual stacking by observations", {
    result <- StackData(findInstDirFile("Cola.sav"),
                        specify.by = "Observation",
                        manual.stacking = c("Q6_A, Q9_A",
                                            "Q6_B, Q9_B",
                                            "Q6_C, Q9_C",
                                            "Q6_D, Q9_D",
                                            "Q6_E, Q9_E",
                                            "Q6_F, Q9_F"))

    md <- result$stacked.data.set.metadata
    expect_true("Q6_" %in% md$variable.names)
    expect_true("Q9_" %in% md$variable.names)
    expect_true(md$is.stacked.variable["Q6_"])
    expect_true(md$is.stacked.variable["Q9_"])
    expect_equal(md$stacking.input.variable.names[["Q6_"]],
                 c("Q6_A", "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F", NA))
    expect_equal(md$stacking.input.variable.names[["Q9_"]],
                 c("Q9_A", "Q9_B", "Q9_C", "Q9_D", "Q9_E", "Q9_F", NA))

    result <- StackData(findInstDirFile("Cola.sav"),
                        specify.by = "Observation",
                        manual.stacking = c("Q5_*_1",
                                            "Q5_*_2",
                                            "Q5_*_3",
                                            "Q5_*_4",
                                            "Q5_*_5",
                                            "Q5_*_6",
                                            "Q5_*_7"))
    md <- result$stacked.data.set.metadata
    v.names <- c("Q5_5_", "Q5_7_", "Q5_13_", "Q5_16_", "Q5_17_", "Q5_19_",
                 "Q5_23_", "Q5_25_", "Q5_31_")
    expect_true(all(v.names %in% md$variable.names))
    expect_true(all(md$is.stacked.variable[v.names]))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Observation",
                             manual.stacking = c("LastResp", "Q3")),
                   paste0("No manual stacking was conducted as the manual ",
                          "stacking input 'Q3' would result in the stacking ",
                          "of variables with mismatching types or value attributes."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Observation",
                             manual.stacking = c("Q6_A, not_a_variable",
                                                 "Q6_B, Q9_B")),
                   paste0("The manual stacking input varible name ",
                          "'not_a_variable' could not be identified. ",
                          "No manual stacking was conducted."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Observation",
                             manual.stacking = c("Q6_A, Q6_A",
                                                 "Q6_B, Q9_B")),
                   paste0("No manual stacking was conducted as the manual ",
                          "stacking input 'Q6_A, Q6_A' contains duplicate ",
                          "entries for 'Q6_A'."), fixed = TRUE)

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Observation",
                             manual.stacking = c("Q6_A, Q9_A",
                                                 "Q6_A, Q9_B")),
                   paste0("No manual stacking was conducted as the manual ",
                          "stacking input 'Q6_A, Q9_B' contains variable(s) ",
                          "that overlap with another manual stacking input ",
                          "'Q6_A, Q9_A'."),
                   fixed = TRUE)
})
