context("Stacking")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

test_that("no stacking", {
    result <- StackData(findInstDirFile("Cola.sav"),
                        stack.with.common.labels = "Disabled")
    expect_equal(result$stacked.data.set.metadata$n.variables, 0)
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
                   paste0("The manual stacking input variable name ",
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
                   paste0("No manual stacking was conducted as the following ",
                          "variables to be stacked have mismatching types or ",
                          "value attributes: LastResp, Q3."))

    expect_warning(StackData(findInstDirFile("Cola.sav"),
                             specify.by = "Observation",
                             manual.stacking = c("Q6_A, not_a_variable",
                                                 "Q6_B, Q9_B")),
                   paste0("The manual stacking input variable name ",
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

test_that("included non-stacked variables", {
    result <- StackData(findInstDirFile("Cola.sav"),
                        variables.to.include = "Q2,Q3",
                        include.stacked.data.set.in.output = TRUE)
    expect_equal(result$stacked.data.set.metadata$variable.names,
                 c("Q1_", "Q2", "Q3", "Q5_5_", "Q5_7_", "Q5_13_", "Q5_16_", "Q5_17_",
                   "Q5_19_", "Q5_23_", "Q5_25_", "Q5_31_", "Q6_", "Q9_",
                   "original_case", "observation"))
    input <- readDataSets(findInstDirFile("Cola.sav"))[[1]]
    expect_equal(as.numeric(result$stacked.data.set$Q2),
                 as.numeric(rep(input$Q2, each = 7)))
})

test_that("automaticCommonLabels", {
    v.names <- c("Q1_A", "Q1_B", "Q1_C", "Q2", "Q3", "Q4_1", "Q4_2", "Q4_3",
                 "Q5_1", "Q5_2", "Q5_3")
    # Note that Diet Coke and Pepsi appear in different orders in Q1 and Q4
    v.labels <- c("Q1. Frequency: Coke", "Q1. Frequency: Diet Coke", "Q1. Frequency: Pepsi",
                  "Q2. Gender", "Q3. Age",
                  "Q4. Do you like: Coke", "Q4. Do you like: Pepsi", "Q4. Do you like: Diet Coke",
                  "Q5. Do you drink: Coffee", "Q5. Do you drink: Tea", "Q5. Do you drink: Sparking Mineral Water")
    common.labels <- automaticCommonLabels(list(variable.names = v.names,
                                                variable.labels = v.labels))
    # "Coke", "Diet Coke", "Pepsi" were chosen over "Coffee", "Tea" and
    # "Sparking Mineral Water" as they appear twice in the labels
    expect_equal(common.labels, list(c("Coke", "Diet Coke", "Pepsi")))
})

test_that("commonLabelsFromReferenceVars", {
    ref.vars.to.stack.text <- c("Q2_A, Q2_B, Q2_C", "Q3_a, Q3_b")
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q2_C", "Q3_a", "Q3_b")
    v.labels <- c("Question 1",  "Q2: Coke", "Q2: Diet Coke", "Q2: Coke Zero",
                  "Question 3: Coca Cola", "Question 3: Diet Coke")
    common.labels.list <- commonLabelsFromReferenceVars(ref.vars.to.stack.text,
                                                        list(variable.names = v.names,
                                                             variable.labels = v.labels))
    expect_equal(common.labels.list,
                 list(c("Coke", "Diet Coke", "Coke Zero"),
                      c("Coca Cola", "Diet Coke")))
})

test_that("commonLabelsFromASetOfReferenceVars", {
    ref.vars.to.stack.text <- "Q2_A, Q2_B, Q2_C"
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q2_C", "Q3_a", "Q3_b")
    v.labels <- c("Question 1",  "Q2: Coke", "Q2: Diet Coke", "Q2: Coke Zero",
                  "Question 3: Coca Cola", "Question 3: Diet Coke")
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

    ref.vars.to.stack.text <- "BAD_VAR_*"
    expect_warning(common.labels <- commonLabelsFromASetOfReferenceVars(ref.vars.to.stack.text,
                                                                        list(variable.names = v.names,
                                                                             variable.labels = v.labels)),
                   paste0("No matches were found for the common labels input ",
                          "wildcard name 'BAD_VAR_*'. Ensure that the wildcard ",
                          "variable name has been correctly specified. Common ",
                          "labels could not be obtained from the input ",
                          "'BAD_VAR_*'."), fixed = TRUE)
    expect_null(common.labels)

    ref.vars.to.stack.text <- "Q2_A"
    expect_warning(common.labels <- commonLabelsFromASetOfReferenceVars(ref.vars.to.stack.text,
                                                                        list(variable.names = v.names,
                                                                             variable.labels = v.labels)),
                   paste0("Only one variable is present in the input 'Q2_A' ",
                          "for extracting common labels. It has been ignored ",
                          "as more than one variable is required."))
    expect_null(common.labels)
})

test_that("stackWithCommonLabels", {
    common.labels.list <- list(c("Coke", "Diet Coke", "Coke Zero"),
                               c("Coca Cola", "Diet Coke"))
    # Multiple identical labels and variables to be stacked not grouped together
    # We have to rely on variable names in this example
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q3_a", "Q4_1", "Q3_b", "Q3_c", "Q4_2",
                 "Q5_1", "Q5_2")
    v.labels <- c("Question 1",
                  "Q2: Coke", "Q2: Diet Coke",
                  "Coke?", "Coke?",
                  "Diet Coke?",
                  "Coke Zero?", "Coke Zero?",
                  "Q5: Coca Cola", "Q5: Diet Coke")
    v.types <- rep("Categorical", length(v.names))
    val.attr <- 1:3
    names(val.attr) <- letters[1:3]
    v.val.attr <- rep(list(val.attr), length(v.names))

    stacking.groups <- stackWithCommonLabels(common.labels.list,
                                             list(variable.names = v.names,
                                                  variable.labels = v.labels,
                                                  variable.types = v.types,
                                                  variable.value.attributes = v.val.attr))

    expect_equal(stacking.groups,
                 structure(c(4L, 5L, 2L, 9L, 6L, NA, 3L, 10L, 7L, 8L, NA, NA),
                           .Dim = 4:3, unstackable.names = list()))

    # Differing variable types
    common.labels.list <- list(c("Coke", "Diet Coke", "Coke Zero"))
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q2_C")
    v.labels <- c("Question 1",
                  "Q2: Coke", "Q2: Diet Coke", "Q2: Coke Zero")
    v.types <- c(rep("Categorical", 2), "Numeric")
    val.attr <- 1:3
    names(val.attr) <- letters[1:3]
    v.val.attr <- rep(list(val.attr), length(v.names))
    expect_warning(stacking.groups <- stackWithCommonLabels(common.labels.list,
                                                            list(variable.names = v.names,
                                                                 variable.labels = v.labels,
                                                                 variable.types = v.types,
                                                                 variable.value.attributes = v.val.attr)),
                   "Some variables could not be stacked due to mismatching ",
                   "variable types or value attributes. See Notes section in ",
                   "output for more details.")
    expect_equal(stacking.groups,
                 structure(integer(0), .Dim = c(0L, 3L),
                           unstackable.names = list(c("Q2_A", "Q2_B", "Q2_C"))))

    # Differing value attributes
    val.attr.2 <- 4:6
    names(val.attr.2) <- letters[4:6]
    v.val.attr <- list(val.attr, val.attr, val.attr.2)
    expect_warning(stacking.groups <- stackWithCommonLabels(common.labels.list,
                                                            list(variable.names = v.names,
                                                                 variable.labels = v.labels,
                                                                 variable.types = v.types,
                                                                 variable.value.attributes = v.val.attr)),
                   "Some variables could not be stacked due to mismatching ",
                   "variable types or value attributes. See Notes section in ",
                   "output for more details.")
    expect_equal(stacking.groups,
                 structure(integer(0), .Dim = c(0L, 3L),
                           unstackable.names = list(c("Q2_A", "Q2_B", "Q2_C"))))
})

test_that("stackingGroupFromCommonLabels", {
    common.labels <- c("Coke", "Diet Coke", "Coke Zero")
    # Multiple identical labels and variables to be stacked not grouped together
    # We have to rely on variable names in this example
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q3_a", "Q4_1", "Q3_b", "Q3_c", "Q4_2")
    v.labels <- c("Question 1",
                  "Q2: Coke", "Q2: Diet Coke",
                  "Coke?", "Coke?",
                  "Diet Coke?",
                  "Coke Zero?", "Coke Zero?")
    stacking.group <- stackingGroupFromCommonLabels(common.labels, v.names, v.labels)
    expect_equal(stacking.group,
                 structure(c(4L, 5L, 2L, 6L, NA, 3L, 7L, 8L, NA),
                           .Dim = c(3L, 3L)))
})

test_that("matchIndicesBasedOnName", {
    ind.list <- list(c(1, 4), c(5, 2), 3)
    nms <- c("Q2_A_X0", "Q2_B_X0", "Q2_C_X0", "Q3_1", "Q3_2")
    result <- matchIndicesBasedOnName(ind.list, nms)
    expect_equal(result,
                 structure(c(1, 4, 2, 5, 3, NA), .Dim = 2:3))
})

test_that("stackManually", {
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q2_C", "Q2_D",
                 "Q3_A", "Q3_B", "Q3_C",
                 "Q4_1", "Q4_2", "Q5")
    v.types <- rep("Categorical", length(v.names))
    val.attr <- 1:3
    names(val.attr) <- letters[1:3]
    v.val.attr <- rep(list(val.attr), length(v.names))

    # Stack by variable
    stacking.groups <- stackManually(c("Q2_A-Q2_D", "Q3_*"),
                                     "Variable",
                                     list(variable.names = v.names,
                                          variable.types = v.types,
                                          variable.value.attributes = v.val.attr))
    expect_equal(stacking.groups,
                 structure(c(2L, 6L, 3L, 7L, 4L, 8L, 5L, NA),
                           .Dim = c(2L, 4L)))

    # Stack by observation
    stacking.groups <- stackManually(c("Q*_A", "Q*_B", "Q*_C", "Q2_D"),
                                     "Observation",
                                     list(variable.names = v.names,
                                          variable.types = v.types,
                                          variable.value.attributes = v.val.attr))
    expect_equal(stacking.groups,
                 structure(c(2L, 6L, 3L, 7L, 4L, 8L, 5L, NA),
                           .Dim = c(2L, 4L)))
})

test_that("stackingSpecifiedByVariable", {
    v.names <- c("Q1", "Q2_A", "Q2_B", "Q2_C", "Q2_D",
                 "Q3_A", "Q3_B", "Q3_C",
                 "Q4_1", "Q4_2", "Q5")
    v.types <- rep("Categorical", length(v.names))
    val.attr <- 1:3
    names(val.attr) <- letters[1:3]
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

    # Bad input variable
    expect_warning(stacking.groups <- stackingSpecifiedByVariable(c("Q2_A-Q2_D", "Q3_A, Q3_B, BAD_VAR"),
                                                   list(variable.names = v.names,
                                                        variable.types = v.types,
                                                        variable.value.attributes = v.val.attr)),
                   paste0("The manual stacking input variable name 'BAD_VAR' ",
                          "could not be identified. The manual stacking input ",
                          "'Q3_A, Q3_B, BAD_VAR' has been ignored."))
    expect_equal(stacking.groups, structure(2:5, .Dim = c(1L, 4L)))

    # Bad input range
    expect_warning(stacking.groups <- stackingSpecifiedByVariable(c("BAD_VAR-Q2_D", "Q3_A, Q3_B, Q3_C"),
                                                                  list(variable.names = v.names,
                                                                       variable.types = v.types,
                                                                       variable.value.attributes = v.val.attr)),
                   paste0("The start variable from the manual stacking input ",
                          "range 'BAD_VAR-Q2_D' could not be identified. ",
                          "The manual stacking input 'BAD_VAR-Q2_D' has been ignored. ",
                          "Ensure that the variable name is correctly specified."))
    expect_equal(stacking.groups, structure(6:8, .Dim = c(1L, 3L)))

    # Incompatible variable types
    v.types.2 <- c(rep("Numeric", 2), rep("Categorical", length(v.names) - 2))
    expect_warning(stacking.groups <- stackingSpecifiedByVariable(c("Q2_A-Q2_D", "Q3_*"),
                                                                  list(variable.names = v.names,
                                                                       variable.types = v.types.2,
                                                                       variable.value.attributes = v.val.attr)),
                   paste0("The manual stacking input 'Q2_A-Q2_D' has been ",
                          "ignored as it contains variables with mismatching ",
                          "types or value attributes."))
    expect_equal(stacking.groups, structure(6:8, .Dim = c(1L, 3L)))

    # Incompatible value attributes
    val.attr.2 <- 4:6
    names(val.attr.2) <- letters[1:3]
    v.val.attr.2 <- v.val.attr
    v.val.attr.2[[2]] <- val.attr.2
    expect_warning(stacking.groups <- stackingSpecifiedByVariable(c("Q2_A-Q2_D", "Q3_*"),
                                                                  list(variable.names = v.names,
                                                                       variable.types = v.types,
                                                                       variable.value.attributes = v.val.attr.2)),
                   paste0("The manual stacking input 'Q2_A-Q2_D' has been ",
                          "ignored as it contains variables with mismatching ",
                          "types or value attributes."))
    expect_equal(stacking.groups, structure(6:8, .Dim = c(1L, 3L)))

    # Value attributes in different order
    v.val.attr.3 <- v.val.attr
    v.val.attr.3[[2]] <- rev(v.val.attr.3[[2]])
    expect_warning(stacking.groups <- stackingSpecifiedByVariable(c("Q2_A-Q2_D", "Q3_*"),
                                                                  list(variable.names = v.names,
                                                                       variable.types = v.types,
                                                                       variable.value.attributes = v.val.attr.3)), NA)
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
    names(val.attr) <- letters[1:3]
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

    # Bad input variable
    expect_warning(stacking.groups <- stackingSpecifiedByObservation(c("Q*_A", "Q*_B", "Q*_C", "BAD_VAR"),
                                                                     list(variable.names = v.names,
                                                                          variable.types = v.types,
                                                                          variable.value.attributes = v.val.attr)),
                   paste0("The manual stacking input variable name 'BAD_VAR' ",
                          "could not be identified. No manual stacking was conducted."))
    expect_null(stacking.groups)

    # Bad input wildcard
    expect_warning(stacking.groups <- stackingSpecifiedByObservation(c("BAD*_VAR", "Q*_B", "Q*_C"),
                                                                     list(variable.names = v.names,
                                                                          variable.types = v.types,
                                                                          variable.value.attributes = v.val.attr)),
                   paste0("No matches were found for the manual stacking input wildcard name 'BAD*_VAR'. ",
                          "Ensure that the wildcard variable name has been correctly specified. ",
                          "No manual stacking was conducted."), fixed = TRUE)
    expect_null(stacking.groups)

    # Incompatible variable types
    v.types.2 <- c(rep("Numeric", 2), rep("Categorical", length(v.names) - 2))
    expect_warning(stacking.groups <- stackingSpecifiedByObservation(c("Q*_A", "Q*_B", "Q*_C", "Q2_D"),
                                                                  list(variable.names = v.names,
                                                                       variable.types = v.types.2,
                                                                       variable.value.attributes = v.val.attr)),
                   paste0("No manual stacking was conducted as the following ",
                          "variables to be stacked have mismatching types or ",
                          "value attributes: Q2_A, Q2_B, Q2_C, Q2_D."))
    expect_null(stacking.groups)

    # Incompatible value attributes
    val.attr.2 <- 4:6
    names(val.attr.2) <- letters[4:6]
    v.val.attr.2 <- v.val.attr
    v.val.attr.2[[2]] <- val.attr.2
    expect_warning(stacking.groups <- stackingSpecifiedByObservation(c("Q*_A", "Q*_B", "Q*_C", "Q2_D"),
                                                                     list(variable.names = v.names,
                                                                          variable.types = v.types.2,
                                                                          variable.value.attributes = v.val.attr)),
                   paste0("No manual stacking was conducted as the following ",
                          "variables to be stacked have mismatching types or ",
                          "value attributes: Q2_A, Q2_B, Q2_C, Q2_D."))
    expect_null(stacking.groups)
})

test_that("mergeCommonLabelAndManualStackingGroups", {
    common.label.stacking.groups <- structure(c(4L, 5L, 6L, NA, 7L, 8L),
                                              .Dim = 2:3, unstackable.names = list(c("Q9", "Q10")))
    manual.stacking.groups <- structure(c(11, 12 , 13),
                                        .Dim = c(1, 3))
    stacking.groups <- mergeCommonLabelAndManualStackingGroups(common.label.stacking.groups,
                                                               manual.stacking.groups)
    expect_equal(stacking.groups,
                 structure(c(4, 5, 11, 6, NA, 12, 7, 8, 13), .Dim = c(3L, 3L),
                           is.manually.stacked = c(FALSE, FALSE, TRUE),
                           unstackable.names = list(c("Q9", "Q10"))))

    expect_equal(mergeCommonLabelAndManualStackingGroups(common.label.stacking.groups, NULL),
                 structure(c(4L, 5L, 6L, NA, 7L, 8L), .Dim = 2:3, unstackable.names = list(
                     c("Q9", "Q10")), is.manually.stacked = c(FALSE, FALSE)))

    expect_equal(mergeCommonLabelAndManualStackingGroups(NULL, manual.stacking.groups),
                 structure(c(11, 12, 13), .Dim = c(1L, 3L), is.manually.stacked = TRUE))
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

test_that("parseVariableName", {
    expect_equal("Q2", parseVariableName("Q2", c("Q1", "Q2", "Q3"),
                                         "unit test", "No warning expected."))

    # Bad input variable
    expect_warning(result <- parseVariableName("BAD_VAR", c("Q1", "Q2", "Q3"),
                                               "unit test", "Warning expected."),
                   paste0("The unit test input variable name 'BAD_VAR' could not be identified. ",
                          "Warning expected."))
    expect_equal(result, structure(character(0), is.not.found = TRUE))
})

test_that("parseVariableRange", {
    expect_equal(c("Q2_A", "Q2_B", "Q3"),
                 parseVariableRange("Q2_A-Q3", c("Q1", "Q2_A", "Q2_B", "Q3", "Q4"),
                                    "unit test", "No warning expected."))

    # Bad start variable
    expect_warning(result <- parseVariableRange("BAD_VAR-Q3", c("Q1", "Q2_A", "Q2_B", "Q3", "Q4"),
                                                "unit test", "Warning expected."),
                   paste0("The start variable from the unit test input range ",
                          "'BAD_VAR-Q3' could not be identified. Warning expected. ",
                          "Ensure that the variable name is correctly specified."))
    expect_equal(result, structure(character(0), is.not.found = TRUE))

    # Bad end variable
    expect_warning(result <- parseVariableRange("Q3-BAD_VAR", c("Q1", "Q2_A", "Q2_B", "Q3", "Q4"),
                                                "unit test", "Warning expected."),
                   paste0("The end variable from the unit test input range ",
                          "'Q3-BAD_VAR' could not be identified. Warning expected. ",
                          "Ensure that the variable name is correctly specified."))
    expect_equal(result, structure(character(0), is.not.found = TRUE))

    # Unsupported wildcard character
    expect_warning(result <- parseVariableRange("Q1-Q2_*", c("Q1", "Q2_A", "Q2_B", "Q3", "Q4"),
                                                "unit test", "Warning expected."),
                   paste0("The end variable from the unit test input range ",
                          "'Q1-Q2_*' contains the wildcard character '*' ",
                          "which is not permitted in a range. Warning expected."), fixed = TRUE)
    expect_equal(result, character(0))

    # Start variable appears after end variable
    expect_warning(result <- parseVariableRange("Q3-Q1", c("Q1", "Q2_A", "Q2_B", "Q3", "Q4"),
                                                "unit test", "Warning expected."),
                   paste0("The start variable from the unit testinput range ",
                          "'Q3-Q1' appears after the end variable in the data set. ",
                          "Ensure that the range has been correctly specified. ",
                          "Warning expected."))
    expect_equal(result, character(0))
})

test_that("parseVariableWildcard", {
    expect_equal(c("Q2_A_X", "Q2_B_X"),
                 parseVariableWildcard("Q2_*_X", c("Q1", "Q2_A_X", "Q2_A_Y",
                                                   "Q2_B_X", "Q2_B_Y", "Q3"),
                                       "unit test", "No warning expected"))

    # Bad wildcard input
    expect_warning(result <- parseVariableWildcard("BAD_*_VAR", c("Q1", "Q2_A_X", "Q2_A_Y",
                                                               "Q2_B_X", "Q2_B_Y", "Q3"),
                                                   "unit test", "Warning expected"),
                   paste0("No matches were found for the unit test input ",
                          "wildcard name 'BAD_*_VAR'. Ensure that the ",
                          "wildcard variable name has been correctly specified. ",
                          "Warning expected"), fixed = TRUE)
})

test_that("isValueAttributesMergable", {
    val.attrs <- list(structure(1:3, .Names = c("A", "B", "C")),
                      structure(4:6, .Names = c("D", "E", "F")))
    expect_true(isValueAttributesMergable(val.attrs))

    val.attrs <- list(structure(1:3, .Names = c("A", "B", "C")),
                      structure(1:3, .Names = c("A", "B", "C")))
    expect_true(isValueAttributesMergable(val.attrs))

    val.attrs <- list(structure(1:3, .Names = c("A", "B", "C")),
                      structure(1:3, .Names = c("D", "E", "F")))
    expect_false(isValueAttributesMergable(val.attrs))

    val.attrs <- list(structure(1:3, .Names = c("A", "B", "C")),
                      structure(4:6, .Names = c("A", "B", "C")))
    expect_false(isValueAttributesMergable(val.attrs))
})

test_that("stackedValueAttributes", {
    val.attrs <- list(structure(4:6, .Names = c("D", "E", "F")),
                      structure(1:3, .Names = c("A", "B", "C")))
    expect_equal(stackedValueAttributes(1:2, val.attrs),
                 structure(1:6, .Names = c("A", "B", "C", "D", "E", "F")))

    val.attrs <- list(structure(1:3, .Names = c("A", "B", "C")),
                      structure(1:3, .Names = c("A", "B", "C")))
    expect_equal(stackedValueAttributes(1:2, val.attrs),
                 structure(1:3, .Names = c("A", "B", "C")))
})


# cola19.sav was created from cola.sav by appending @ to the end of the variable names of the 1st question
test_that("DS-3758: special characters at end of variable names", {
    expect_error(StackData(findInstDirFile("cola19.sav"),
                           stack.with.common.labels = "Disabled",
                           specify.by = "Variable",
                           manual.stacking = c("Q1_F_c@,Q1_E_c1@,Q1_D_c@,Q1_C_c1@,Q1_A_c@,Q1_B_c1@")), NA)
})

# cola20.sav has variables with the same labels, e.g. Q10_A, Q10_A_2.
test_that("DS-3781: identical variable labels", {
    result <- StackData(findInstDirFile("cola20.sav"),
                        stack.with.common.labels = "Disabled",
                        specify.by = "Variable",
                        manual.stacking = c("Q10_A,Q10_A_2", "Q10_B,Q10_B_2"))
    expect_equal(unname(result$stacked.data.set.metadata$variable.labels[1]),
                 "Sometimes I drink cola that is bought by my friends or family")
    expect_equal(unname(result$stacked.data.set.metadata$variable.labels[2]),
                 "Three words only")
})

# Clean up stacked files
if (file.exists("cola19 stacked.sav"))
    file.remove("cola19 stacked.sav")

if (file.exists("cola20 stacked.sav"))
    file.remove("cola20 stacked.sav")