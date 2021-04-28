context("Stacking")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

test_that("no stacking", {
    result <- StackData(findInstDirFile("Cola.sav"),
                        stack.with.common.labels = "Disabled")
    expect_false(any(result$stacked.data.set.metadata$is.stacked.variable))
    expect_equal(result$stacked.data.set.metadata$n.variables, 198)
})

test_that("common label stacking", {
    result.auto <- StackData(findInstDirFile("Cola.sav"))

    common.labels <- list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                            "Diet Pepsi", "Pepsi Max", "None of these"))
    result <- StackData(findInstDirFile("Cola.sav"), manual.common.labels = common.labels,
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
                        reference.variables.to.stack = "Q5_5_*",
                        include.stacked.data.set.in.output = TRUE)
    expect_true(setequal(result.auto$stacked.data.set.metadata$variable.names,
                         result$stacked.data.set.metadata$variable.names))
    expect_equal(attr(result$stacked.data.set$observation, "labels"),
                 structure(1:7, .Names = c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                                           "Diet Pepsi", "Pepsi Max", "None of these")))

    result <- StackData(findInstDirFile("Cola.sav"),
                        include.original.case.variable = FALSE)
    expect_false("original_case" %in% result$stacked.data.set.metadata$variable.names)

    result <- StackData(findInstDirFile("Cola.sav"),
                        include.observation.variable = FALSE)
    expect_false("observation" %in% result$stacked.data.set.metadata$variable.names)
})

test_that("Multiple common labels", {
    common.labels <- list(c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi",
                            "Pepsi Light", "Pepsi Max"),
                          c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                            "Diet Pepsi", "Pepsi Max", "None of these"))
    result <- StackData(findInstDirFile("Cola.sav"), manual.common.labels = common.labels,
                        stack.with.common.labels = "Using manually input common labels")
    expect_equal(result$stacked.data.set.metadata$stacking.input.variable.names$Q6,
                 c("Q6_A", "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F", NA))
    expect_equal(result$stacked.data.set.metadata$stacking.input.variable.names$Q9,
                 c("Q9_A", "Q9_B", "Q9_C", "Q9_D", "Q9_E", "Q9_F", NA))

    result <- StackData(findInstDirFile("Cola.sav"),
                        stack.with.common.labels = "Using a set of variables to stack as reference",
                        reference.variables.to.stack = c("Q5_5_*", "Q6_*"))
    expect_equal(result$stacked.data.set.metadata$stacking.input.variable.names$Q6,
                 c("Q6_A", "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F", NA))
    expect_equal(result$stacked.data.set.metadata$stacking.input.variable.names$Q9,
                 c("Q9_A", "Q9_B", "Q9_C", "Q9_D", "Q9_E", "Q9_F", NA))
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

test_that("commonLabelsFromASetOfReferenceVars", {
    ref.vars.to.stack.text <- "Q2_A, Q2_B, Q2_C"
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q2_C", "Q3_a", "Q3_b")
    v.labels <- c("Question 1",  "Q2: Coke", "Q2: Diet Coke", "Q2: Coke Zero",
                  "Question 3a", "Question 3b")
    common.labels <- commonLabelsFromASetOfReferenceVars(ref.vars.to.stack.text,
                                                         list(variable.names = v.names,
                                                              variable.labels = v.labels))
    expect_equal(common.labels, c("Coke", "Diet Coke", "Coke Zero"))

    ref.vars.to.stack.text <- "Q2_A-Q2_C"
    common.labels <- commonLabelsFromASetOfReferenceVars(ref.vars.to.stack.text,
                                                         list(variable.names = v.names,
                                                              variable.labels = v.labels))
    expect_equal(common.labels, c("Coke", "Diet Coke", "Coke Zero"))

    ref.vars.to.stack.text <- "Q2_*"
    common.labels <- commonLabelsFromASetOfReferenceVars(ref.vars.to.stack.text,
                                                         list(variable.names = v.names,
                                                              variable.labels = v.labels))
    expect_equal(common.labels, c("Coke", "Diet Coke", "Coke Zero"))

    ref.vars.to.stack.text <- "bad_var_*"
    expect_warning(commonLabelsFromASetOfReferenceVars(ref.vars.to.stack.text,
                                                       list(variable.names = v.names,
                                                            variable.labels = v.labels)),
                   paste0("No matches were found for the common labels input ",
                          "wildcard name 'bad_var_*'. Ensure that the wildcard ",
                          "variable name has been correctly specified. Common ",
                          "labels could not be obtained from the input ",
                          "'bad_var_*'."), fixed = TRUE)

    ref.vars.to.stack.text <- "Q2_A"
    expect_warning(commonLabelsFromASetOfReferenceVars(ref.vars.to.stack.text,
                                                       list(variable.names = v.names,
                                                            variable.labels = v.labels)),
                   paste0("Only one variable is present in the input 'Q2_A' ",
                          "for extracting common labels. It has been ignored ",
                          "as more than one variable is required."))
})

test_that("stackingSpecifiedByVariable", {
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q2_C", "Q2_D",
                 "Q3_A", "Q3_B", "Q3_C",
                 "Q4_1", "Q4_2", "Q5")
    v.types <- rep("Categorical", length(v.names))
    val.attr <- 1:3
    names(val.attr) <- letter[1:3]
    v.val.attr <- rep(list(val.attr), length(v.names))

    stacking.groups <- stackingSpecifiedByVariable(c("Q2_A-Q2_D", "Q3_*"),
                                                   list(variable.names = v.names,
                                                        variable.types = v.types,
                                                        variable.value.attributes = v.val.attr))
    expect_equal(stacking.groups,
                 structure(c(2L, 6L, 3L, 7L, 4L, 8L, 5L, NA),
                           .Dim = c(2L, 4L)))

    # Include NA
    stacking.groups <- stackingSpecifiedByVariable(c("Q2_A-Q2_D", "Q3_A, Q3_B, N/A, Q3_C"),
                                                   list(variable.names = v.names,
                                                        variable.types = v.types,
                                                        variable.value.attributes = v.val.attr))
    expect_equal(stacking.groups,
                 structure(c(2L, 6L, 3L, 7L, 4L, NA, 5L, 8L),
                           .Dim = c(2L, 4L)))

    stacking.groups <- stackManually(c("Q2_A-Q2_D", "Q3_*"),
                                     "Variable",
                                     list(variable.names = v.names,
                                          variable.types = v.types,
                                          variable.value.attributes = v.val.attr))
    expect_equal(stacking.groups,
                 structure(c(2L, 6L, 3L, 7L, 4L, 8L, 5L, NA),
                           .Dim = c(2L, 4L)))
})

test_that("stackingSpecifiedByObservation", {
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q2_C", "Q2_D",
                 "Q3_A", "Q3_B", "Q3_C",
                 "Q4_1", "Q4_2", "Q5")
    v.types <- rep("Categorical", length(v.names))
    val.attr <- 1:3
    names(val.attr) <- letter[1:3]
    v.val.attr <- rep(list(val.attr), length(v.names))

    stacking.groups <- stackingSpecifiedByObservation(c("Q*_A", "Q*_B", "Q*_C", "Q2_D"),
                                                      list(variable.names = v.names,
                                                           variable.types = v.types,
                                                           variable.value.attributes = v.val.attr))
    expect_equal(stacking.groups,
                 structure(c(2L, 6L, 3L, 7L, 4L, 8L, 5L, NA),
                           .Dim = c(2L, 4L)))

    # Include N/A
    stacking.groups <- stackingSpecifiedByObservation(c("Q*_A", "Q*_B", "Q2_C, N/A", "Q2_D, Q3_C"),
                                                      list(variable.names = v.names,
                                                           variable.types = v.types,
                                                           variable.value.attributes = v.val.attr))
    expect_equal(stacking.groups,
                 structure(c(2L, 6L, 3L, 7L, 4L, NA, 5L, 8L),
                           .Dim = c(2L, 4L)))

    stacking.groups <- stackManually(c("Q2_A-Q2_D", "Q3_*"),
                                     "Observation",
                                     list(variable.names = v.names,
                                          variable.types = v.types,
                                          variable.value.attributes = v.val.attr))
    expect_equal(stacking.groups,
                 structure(c(2L, 6L, 3L, 7L, 4L, 8L, 5L, NA),
                           .Dim = c(2L, 4L)))
})

test_that("permittedNA", {
    expect_warning(permitted.na <- permittedNA(c("Q1", "Q2", "NA", "Q3")),
                   paste0("There is an input variable named 'NA'. ",
                          "To avoid confusion, missing stacking variables ",
                          "need to be specified with an extra slash for ",
                          "this data set, i.e., N/A"))
    expect_equal(permitted.na, "N/A")
    expect_equal(permittedNA(c("Q1", "Q2", "Q3")), c("NA", "N/A"))
})
