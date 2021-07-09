context("MergeDataSetsByCase")

library(gtools)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

# cola1.sav: Q2 (Q2. Gender) removed, Q3_3 (Q3. Age in years) moved down to be
#            after Q4_C_2. Variables after Q4_C_4 removed.
# cola2.sav: Q2 (Q2. Gender) category labels changed to M, F, and
#            Q3_3 (Q3. Age in years) category values changed to 1-10.
#            Variables after Q4_C_4 removed.
# cola3.sav: Q1.  Fragments coded variables removed, variables after Q4_C_2
#            removed except for Q1_F (Q1.  Fragments - Coke) which is a text variable.
# cola4.sav: same as cola3.sav except that Q3_3 renamed to Q3_3_new_name,
# cola5.sav: Q4_A_3, Q4_B_2, Q4_C_2 renamed to Q4_A_3_new, Q4_B_2_new, Q4_C_2_new,
#            variables after Q4_C_4 removed.
# cola6.sav: Q2 (Q2. Gender) converted to numeric
# cola7.sav: Q11_l (text variable) renamed to Q3_3
# cola8.sav: Q1_F_c renamed to Q1F_c
# cola9.sav: Only keep Q1_*, Q3_3, Q4_A_3, Q4_B_2, Q4_C_2, with "Q" in variable
#            names replaced with "Question".
# cola10.sav: Only keep Q1_*, Q3_3, Q4_A_3, Q4_B_2, Q4_C_2.
# cola11.sav: Only keep Q1_*, Q3_3, Q4_A_3, Q4_B_2, Q4_C_2, labels for Q1_*
#             are lower-case, Q3_3 renamed to Q33 and underscores removed from Q4*.
# cola12.sav: Only keep Q1_*, Q3_3, Q4_A_3, Q4_B_2, Q4_C_2. "Coca Cola"
#             replaced with "Coke" in Q4_A_3 label. Value labels
#             "2 - 3 days a week" and "4 - 5 days a week".
# cola13.sav: Includes Date variable LastResp_date.
# cola14.sav: Includes text variable LastResp_date_text containing strings that can be parsed
#             as dates.
#
# To update the merge.data.set.output.rda test data in flipFormat,
# simply save merge.data.set.output from this test
# into flipFormat/inst/testdata/merge.data.set.output.rda
test_that("Example used for widget test in flipFormat", {
    expect_error(merge.data.set.output <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                                                 findInstDirFile("cola2.sav"),
                                                                                 findInstDirFile("cola5.sav"),
                                                                                 findInstDirFile("cola8.sav")),
                                                              data.sets.whose.variables.are.kept = 1,
                                                              variables.to.combine = "Q4_A_3,Q4_A_3_new"), NA)
})

test_that("MergeDataSetsByCase using default settings", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  include.merged.data.set.in.output = TRUE)

    merged.data.set <- result$merged.data.set
    expect_equivalent(merged.data.set$mergesrc,
                      structure(rep(1:3, each = 327), label = "Source of cases",
                                labels = structure(1:3, .Names = c("cola1.sav", "cola2.sav", "cola3.sav")),
                                class = c("integer", "haven_labelled")))
    expect_equal(attr(merged.data.set$mergesrc, "label"), "Source of cases")
    expect_equal(names(merged.data.set), c("Q1_F_c", "Q1_E_c1", "Q1_D_c",
                                           "Q1_C_c1", "Q1_A_c", "Q1_B_c1",
                                           "Q2", "Q3", "Q4_A", "Q4_B", "Q4_C",
                                           "Q4_A_3", "Q4_B_2", "Q4_C_2",
                                           "Q3_3", "Q4_A_5", "Q4_B_3",
                                           "Q4_C_3", "Q4_A_6", "Q4_B_4",
                                           "Q4_C_4", "Q1_F", "mergesrc"))
    expect_equal(vapply(merged.data.set, attr, character(1), "label"),
                 c(Q1_F_c = "Coke", Q1_E_c1 = "Diet Coke", Q1_D_c = "Coke Zero",
                   Q1_C_c1 = "Pepsi", Q1_A_c = "Diet Pepsi", Q1_B_c1 = "Pepsi Max",
                   Q2 = "Q2. Gender", Q3 = "Q3. Age", Q4_A = "Colas (e.g., Coca Cola, Pepsi Max)?",
                   Q4_B = "Sparkling mineral water", Q4_C = "Coffee", Q4_A_3 = "Colas (e.g., Coca Cola, Pepsi Max)?",
                   Q4_B_2 = "Sparkling mineral water", Q4_C_2 = "Coffee", Q3_3 = "Q3. Age in years",
                   Q4_A_5 = "Colas (e.g., Coca Cola, Pepsi Max)?", Q4_B_3 = "Sparkling mineral water",
                   Q4_C_3 = "Coffee", Q4_A_6 = "Q4.  Frequency numeric: Colas (e.g., Coca Cola, Pepsi Max)?",
                   Q4_B_4 = "Q4.  Frequency numeric: Sparkling mineral water", Q4_C_4 = "Q4.  Frequency numeric: Coffee",
                   Q1_F = "Q1.  Fragments - Coke", mergesrc = "Source of cases"))

    expect_true(all(is.na(merged.data.set$Q2[1:327]))) # 1st data set does not have Q2
    # New values were created since multiple labels have same value
    expect_equal(unique(merged.data.set$Q2[328:981]), c(2,1,4,3))
    expect_equal(attr(merged.data.set$Q2, "labels"),
                 structure(1:4, .Names = c("M", "F", "Male", "Female")))

    # Values in Q3_3 of the 2nd data set are modified (keeping the same value labels).
    # In the merged data set, the values and value labels of the first data set are used
    expect_true(all(merged.data.set$Q3_3[328:654] == merged.data.set$Q3_3[1:327]))
    expect_equal(attr(merged.data.set$Q3_3, "labels"),
                 c(`Less than 18` = 18, `18 to 24` = 22, `25 to 29` = 27, `30 to 34` = 33,
                   `35 to 39` = 37, `40 to 44` = 42, `45 to 49` = 47, `50 to 54` = 52,
                   `55 to 64` = 60, `65 or more` = 70))
})

test_that("Automatically determine match", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")))
    match.pars <- attr(result$matched.names, "match.parameters")
    # Only match by variable names due to duplication in variable attributes
    # and value labels
    expect_true(match.pars$match.by.variable.names)
    expect_false(match.pars$match.by.variable.labels)
    expect_false(match.pars$match.by.value.labels)

    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola9.sav"),
                                                     findInstDirFile("cola10.sav")))
    match.pars <- attr(result$matched.names, "match.parameters")
    # Only match by variable labels since variable labels are not duplicated,
    # variable names do not match and value attributes are duplicated.
    expect_false(match.pars$match.by.variable.names)
    expect_true(match.pars$match.by.variable.labels)
    expect_false(match.pars$match.by.value.labels)
})

test_that("Matching by variable labels", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola7.sav"),
                                                     findInstDirFile("cola8.sav")),
                                  auto.select.what.to.match.by = FALSE,
                                  match.by.variable.names = FALSE,
                                  match.by.variable.labels = TRUE,
                                  match.by.value.labels = FALSE,
                                  include.merged.data.set.in.output = TRUE)
    # Q1_F_c matched despite variable names being different
    expect_true(all(!is.na(result$merged.data.set$Q1_F_c)))
})

test_that("Matching by value labels", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola7.sav"),
                                                     findInstDirFile("cola8.sav")),
                                  auto.select.what.to.match.by = FALSE,
                                  match.by.variable.names = FALSE,
                                  match.by.variable.labels = FALSE,
                                  match.by.value.labels = TRUE,
                                  include.merged.data.set.in.output = TRUE,
                                  variables.to.omit = "Q1_E_c1-Q1_B_c1")
    # Q1_F_c matched despite variable names being different
    expect_true(all(!is.na(result$merged.data.set$Q1_F_c)))
})

test_that("Ignore case when matching", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola10.sav"),
                                                     findInstDirFile("cola11.sav")))
    # Lowercase and non-lowercase variable labels are matched
    expect_true(all(!is.na(result$merged.data.set$Q1_F_c)))
    expect_true(all(!is.na(result$merged.data.set$Q1_E_c1)))
})

test_that("Ignore non-alphanumeric characters when matching", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola10.sav"),
                                                     findInstDirFile("cola11.sav")),
                                  auto.select.what.to.match.by = FALSE,
                                  match.by.variable.names = TRUE,
                                  match.by.variable.labels = FALSE,
                                  match.by.value.labels = FALSE,
                                  include.merged.data.set.in.output = TRUE)
    expect_true("Q3_3" %in% names(result$merged.data.set))
    expect_true("Q33" %in% names(result$merged.data.set)) # not merged into Q3_3 as numbers not preserved
    expect_false("Q4A3" %in% names(result$merged.data.set)) # does not exist since merged into Q4_A_3
    expect_true(all(!is.na(result$merged.data.set$Q4_A_3)))
})

test_that("Minimum match percentage", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola10.sav"),
                                                     findInstDirFile("cola12.sav")),
                                  auto.select.what.to.match.by = FALSE,
                                  match.by.variable.names = FALSE,
                                  match.by.variable.labels = TRUE,
                                  match.by.value.labels = FALSE,
                                  min.match.percentage = 90,
                                  include.merged.data.set.in.output = TRUE)
    # Q4_A_3 not merged together since "Coke" differs from "Coca Cola" beyond
    # the min match percentage
    expect_true("Q4_A_3_1" %in% names(result$merged.data.set))

    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola10.sav"),
                                                     findInstDirFile("cola12.sav")),
                                  auto.select.what.to.match.by = FALSE,
                                  match.by.variable.names = FALSE,
                                  match.by.variable.labels = TRUE,
                                  match.by.value.labels = FALSE,
                                  min.match.percentage = 70, # lowered from 90
                                  include.merged.data.set.in.output = TRUE)
    # Q4_A_3 merged together
    expect_false("Q4_A_3_1" %in% names(result$merged.data.set))
    expect_true(all(!is.na(result$merged.data.set$Q4_A_3)))
})

test_that("Minimum value label match percentage", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola10.sav"),
                                                     findInstDirFile("cola12.sav")),
                                  include.merged.data.set.in.output = TRUE)
    # The differing value labels are not matched together
    expect_true("2 to 3 days a week" %in%
                names(attr(result$merged.data.set$Q4_A_3, "labels")))
    expect_true("2 - 3 days a week" %in%
                names(attr(result$merged.data.set$Q4_A_3, "labels")))

    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola10.sav"),
                                                     findInstDirFile("cola12.sav")),
                                  min.value.label.match.percentage = 80, # lowered from 90
                                  include.merged.data.set.in.output = TRUE)
    # The differing value labels are matched together after lowering
    # min.value.label.match.percentage to 80
    expect_false("2 - 3 days a week" %in%
                 names(attr(result$merged.data.set$Q4_A_3, "labels")))
    expect_equal(attr(result$merged.data.set$Q4_A_3, "labels"),
                 structure(1:9,
                           .Names = c("Never", "Once or twice a year",
                                      "Once every 3 months", "Once a month",
                                      "Once every 2 weeks", "Once a week",
                                      "2 to 3 days a week", "4 to 5 days a week",
                                      "Every or nearly every day")))
})

test_that("Use first label when a single categorical value has multiple labels", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  include.merged.data.set.in.output = TRUE,
                                  when.multiple.labels.for.one.value = "Use one of the labels")
    merged.data.set <- result$merged.data.set
    expect_true(all(is.na(merged.data.set$Q2[1:327]))) # 1st data set does not have Q2
    expect_equal(unique(merged.data.set$Q2[328:981]), c(2,1)) # no new values created
    expect_equal(attr(merged.data.set$Q2, "labels"), c(M = 1, F = 2)) # labels from data set 2 used
})

test_that("Manually combine variables by specifying names of variables to combine", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola4.sav")),
                                  include.merged.data.set.in.output = TRUE,
                                  variables.to.combine = "Q3_3,Q3_3_new_name")
    merged.data.set <- result$merged.data.set
    expect_true("Q3_3" %in% names(merged.data.set))
    expect_false("Q3_3_new_name" %in% names(merged.data.set))
    expect_true(all(!is.na(merged.data.set$Q3_3)))
})

test_that("Manually combine variables by specifying names of variables (with data set index) to combine", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola4.sav")),
                                           include.merged.data.set.in.output = TRUE,
                                           variables.to.combine = "Q3_3_new_name(3),Q3_3(2)")
    merged.data.set <- result$merged.data.set
    expect_true("Q3_3" %in% names(merged.data.set))
    # new variable created with Q3_3 from data set 1 since only Q3_3 from
    # data set 2 was specified to be merged with Q3_3_new_name
    expect_true("Q3_3_1" %in% names(merged.data.set))
    expect_false("Q3_3_new_name" %in% names(merged.data.set))
    expect_true(all(is.na(merged.data.set$Q3_3[1:327])))
    expect_true(all(!is.na(merged.data.set$Q3_3[328:981])))
    expect_true(all(!is.na(merged.data.set$Q3_3_1[1:327])))
    expect_true(all(is.na(merged.data.set$Q3_3_1[328:981])))
})

test_that("Manually combine variables by specifying ranges of variables to combine", {
    # Range
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola5.sav")),
                                  include.merged.data.set.in.output = TRUE,
                                  variables.to.combine = "Q4_A_3 - Q4_C_2,Q4_A_3_new-Q4_C_2_new")
    merged.data.set <- result$merged.data.set
    expect_true(all(c("Q4_A_3", "Q4_B_2", "Q4_C_2") %in% names(merged.data.set)))
    expect_false(any(c("Q4_A_3_new", "Q4_B_2_new", "Q4_C_2_new") %in% names(merged.data.set)))
    expect_true(all(!is.na(merged.data.set$Q4_A_3)))
    expect_true(all(!is.na(merged.data.set$Q4_B_2)))
    expect_true(all(!is.na(merged.data.set$Q4_C_2)))
})

test_that("Manually combine variables by specifying ranges of variables (with data set index) to combine", {
    # Range with data set indices
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola5.sav")),
                                  include.merged.data.set.in.output = TRUE,
                                  variables.to.combine = "Q4_A_3(1) - Q4_C_2,Q4_A_3_new(2)-Q4_C_2_new(2)")
    merged.data.set <- result$merged.data.set
    expect_true(all(c("Q4_A_3", "Q4_B_2", "Q4_C_2") %in% names(merged.data.set)))
    expect_false(any(c("Q4_A_3_new", "Q4_B_2_new", "Q4_C_2_new") %in% names(merged.data.set)))
    expect_true(all(!is.na(merged.data.set$Q4_A_3_new)))
    expect_true(all(!is.na(merged.data.set$Q4_B_2_new)))
    expect_true(all(!is.na(merged.data.set$Q4_C_2_new)))
})

test_that("Error when combining ranges with different numbers of variables", {
    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                           findInstDirFile("cola5.sav")),
                                     variables.to.combine = "Q4_A_3 - Q4_B_2,Q4_A_3_new-Q4_C_2_new"),
                 paste0("The input 'Q4_A_3 - Q4_B_2,Q4_A_3_new-Q4_C_2_new' ",
                        "contains variable ranges with differing numbers of variables. ",
                        "Ensure that the ranges have been correctly specified so that ",
                        "they all contain the same number of variables."))
})

test_that("Error when only one variable specified for variables to combine", {
    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola4.sav")),
                                     variables.to.combine = "Q3_3"),
                 paste0("The input 'Q3_3' for 'Variables to manually combine' only specifies variables from one data set. ",
                        "It needs to specify variables from two or more data sets."))
})

test_that("Error when same variable specified to be combined and omitted", {
    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     variables.to.combine = "Q3_3,Q3_3_new_name",
                                     variables.to.omit = "Q3_3"),
                 paste0("The variable 'Q3_3' has been specified to be both combined and omitted. ",
                        "Ensure that it is specified to be either combined or omitted."))
})

test_that("Error when same variable specified to be combined in multiple inputs", {
    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     variables.to.combine = c("Q3_3,Q3_3_new_name", "Q3_3,Q2")),
                 paste0("The variable 'Q3_3' has been specified to be combined in multiple inputs: ",
                        "'Q3_3,Q3_3_new_name', 'Q3_3,Q2'. Ensure that any of ",
                        "the variables to be combined are specified in at most one input."))
})

test_that("Error when variable to be combined not found", {
    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     variables.to.combine = "Q4_3,Q3_3_new_name"),
                 paste0("The input variable 'Q4_3' could not be found in any ",
                        "of the input data sets. Ensure that the variable ",
                        "has been correctly specified."))
})

test_that("Error when range to be combined not found", {
    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     variables.to.combine = "Q2-Q4,Q2_new-Q4_new"),
                 paste0("The input range 'Q2-Q4' was not found in any of ",
                        "the input data sets. Ensure that the range has ",
                        "been correctly specified."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     variables.to.combine = "Q2-Q3-Q4,Q2_new-Q3_new-Q4_new"),
                 paste0("The input range 'Q2-Q3-Q4' was not found in any of ",
                        "the input data sets. Ensure that the range has been ",
                        "correctly specified."), fixed = TRUE)
})

test_that("Error when two variables to be combined are found in the same data set", {
    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola5.sav")),
                                     variables.to.combine = "Q4_A_3,Q3"),
                 paste0("The input 'Q4_A_3,Q3' for 'Variables to manually combine' ",
                        "contains variables which are both present in data set 1. ",
                        "Each input may only specify at most one variable or ",
                        "variable range per data set. Try explicitly specifying ",
                        "the data set index for variables by appending '(x)' to ",
                        "the variable name, where 'x' is replaced with the data ",
                        "set index, e.g., use '(2)' for the 2nd input data set."),
                 fixed = TRUE)
})

test_that("Variables to not combine", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  variables.to.not.combine = "Q3")
    expect_true(all(c("Q3", "Q3_1", "Q3_2") %in% result$merged.data.set.metadata$variable.names))
})

test_that("Variables to include", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  auto.select.what.to.match.by = FALSE,
                                  include.merged.data.set.in.output = TRUE,
                                  data.sets.whose.variables.are.kept = 3,
                                  variables.to.keep = "Q1_F_c - Q1_D_c, Q1_B_c1")
    # Kept despite not being in cola3.sav since specified to be kept
    expect_true("Q1_F_c" %in% names(result$merged.data.set))
    expect_true("Q1_E_c1" %in% names(result$merged.data.set))
    expect_true("Q1_D_c" %in% names(result$merged.data.set))
    expect_true("Q1_B_c1" %in% names(result$merged.data.set))
    # Omitted as not in cola3.sav and not specified to be kept
    expect_false("Q1_C_c1" %in% names(result$merged.data.set))
})

test_that("Variables to include by specifying wildcard", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  auto.select.what.to.match.by = FALSE,
                                  include.merged.data.set.in.output = TRUE,
                                  data.sets.whose.variables.are.kept = 3,
                                  variables.to.keep = "Q1_*")
    expect_true(all(c("Q1_F_c", "Q1_E_c1", "Q1_D_c", "Q1_C_c1", "Q1_A_c", "Q1_B_c1") %in%
                        names(result$merged.data.set)))
})

test_that("Variables to omit", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  include.merged.data.set.in.output = TRUE,
                                  variables.to.omit = c("Q2(2)", "Q1_F(3)"))
    merged.data.set <- result$merged.data.set
    expect_true(all(is.na(merged.data.set$Q2[1:654])))
    expect_true(all(!is.na(merged.data.set$Q2[655:981])))
})

test_that("Variables to omit by specifying wildcard", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  include.merged.data.set.in.output = TRUE,
                                  variables.to.omit = "Q1_*")
    expect_true(!any(c("Q1_F_c", "Q1_E_c1", "Q1_D_c", "Q1_C_c1", "Q1_A_c", "Q1_B_c1") %in%
                     names(result$merged.data.set)))
})

test_that("Error Variables to omit by specifying wildcard", {
    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola2.sav")),
                                     variables.to.omit = "bad_var"),
                 paste0("The input variable 'bad_var' could not be found in ",
                        "any of the input data sets. Ensure that the ",
                        "variable has been correctly specified."),
                 fixed = TRUE)
})

test_that("Error when only 1 data set is supplied", {
    expect_error(MergeDataSetsByCase(data.set.names = findInstDirFile("cola1.sav")),
                 "At least 2 data set(s) are required.", fixed = TRUE)
})

test_that("Variable type conversion (categorical and numeric to categorical)", {
    # Q2 (gender) is categorical in cola2 and numeric in cola6,
    # merged variable is categorical
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola6.sav")),
                                  include.merged.data.set.in.output = TRUE)
    expect_equal(unname(result$merged.data.set.metadata$variable.types["Q2"]),
                 CATEGORICAL.VARIABLE.TYPE)
    expect_true(all(result$merged.data.set$Q2[1:327] == result$merged.data.set$Q2[328:654]))
})

test_that("Variable type conversion (text to date)", {
    # LastResp_date_text converted to Date
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola13.sav"),
                                                     findInstDirFile("cola14.sav")),
                                  variables.to.combine = "LastResp_date,LastResp_date_text",
                                  include.merged.data.set.in.output = TRUE)
    expect_true(class(result$merged.data.set$LastResp_date) == DATE.VARIABLE.TYPE)
    expect_true(all(!is.na(result$merged.data.set$LastResp_date)))
})

test_that("Non-combinable variables", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola3.sav"),
                                                     findInstDirFile("cola7.sav")),
                                  match.by.variable.names = TRUE,
                                  match.by.variable.labels = FALSE,
                                  match.by.value.labels = FALSE)
    # Q3_3 in the two datasets cannot be combined as the latter is a text
    # variable with many different values. So a new variable Q3_3_1 is created
    # immediately below Q3_3
    expect_true(all(result$merged.data.set.metadata$variable.names[9:10] == c("Q3_3", "Q3_3_1")))
    expect_equal(attr(result$merged.names, "renamed.variables"),
                 structure(c("Q3_3", "Q3_3_1"), .Dim = 1:2,
                           .Dimnames = list(NULL, c("Original name", "New name"))))
})

test_that("Data sets whose variables are kept", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  auto.select.what.to.match.by = FALSE,
                                  include.merged.data.set.in.output = TRUE,
                                  data.sets.whose.variables.are.kept = c(1, 2))
    # Q1_F not present in cola1.sav and cola2.sav
    expect_false("Q1_F" %in% names(result$merged.data.set))

    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  include.merged.data.set.in.output = TRUE,
                                  data.sets.whose.variables.are.kept = 3)
    # Q1_F_c not present in cola3.sav
    expect_false("Q1_F_c" %in% names(result$merged.data.set))

    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  include.merged.data.set.in.output = TRUE,
                                  data.sets.whose.variables.are.kept = 1)
    # Q2 not present in cola1.sav
    expect_false("Q2" %in% names(result$merged.data.set))
})

test_that("maxOneToManyLabelProportion", {
    v.labels <- list(c("Coca-cola", "Diet Coke", "Pepsi"),
                     c("Coca-cola", "Diet Coke", "Coke Zero",
                       "Coca-cola", "Diet Coke", "Coke Zero",
                       "Pepsi"))
    # 2 out of 3 labels in the first vector have more than one match in the
    # second vector
    expect_equal(maxOneToManyLabelProportion(v.labels = v.labels), 0.666666666666667)
})

test_that("maxOneToManyValueAttrProportion", {
    coke.val.attr <- 1:3
    names(coke.val.attr) <- c("Coca-cola", "Diet Coke", "Coke Zero")
    pepsi.val.attr <- 1:2
    names(coke.val.attr) <- c("Pepsi", "Pepsi Max")

    v.val.attrs <- list(list(coke.val.attr, pepsi.val.attr),
                        list(coke.val.attr, coke.val.attr, pepsi.val.attr))
    # 1 out of 2 val.attributes in the first element have more than one match
    # in the second element
    expect_equal(maxOneToManyValueAttrProportion(v.val.attrs = v.val.attrs), 0.5)
})

test_that("parseInputTextIntoVariableNamesMatrix (variable name supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    input.text <- "Q2"
    v.name.matrix <- parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                           input.data.sets.metadata = input.data.sets.metadata,
                                                           allow.wildcards = TRUE,
                                                           input.purpose = "Variables to manually include")

    expect_equal(v.name.matrix,
                 structure(c("Q2", "Q2", NA), .Dim = c(1L, 3L),
                           is.data.set.specified = FALSE))
})

test_that("parseInputTextIntoVariableNamesMatrix (variable name with data set index supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Data set index specified
    input.text <- "Q3(2)"
    v.name.matrix <- parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                           input.data.sets.metadata = input.data.sets.metadata,
                                                           allow.wildcards = TRUE,
                                                           input.purpose = "Variables to manually include")
    expect_equal(v.name.matrix,
                 structure(c(NA, "Q3", NA), .Dim = c(1L, 3L),
                           is.data.set.specified = TRUE))
})

test_that("parseInputTextIntoVariableNamesMatrix (variable range supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Variable range
    input.text <- "Q2-Q4"
    v.name.matrix <- parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                           input.data.sets.metadata = input.data.sets.metadata,
                                                           allow.wildcards = TRUE,
                                                           input.purpose = "Variables to manually include")
    expect_equal(v.name.matrix,
                 structure(c("Q2", "Q3", "Q4", "Q2", "Q3", "Q4", NA, NA, NA),
                           .Dim = c(3L, 3L), is.data.set.specified = FALSE))
})

test_that("parseInputTextIntoVariableNamesMatrix (variable range with data set indices supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Variable range with data set indices
    input.text <- "Q2(1)-Q4(1)"
    v.name.matrix <- parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                           input.data.sets.metadata = input.data.sets.metadata,
                                                           allow.wildcards = TRUE,
                                                           input.purpose = "Variables to manually include")
    expect_equal(v.name.matrix,
                 structure(c("Q2", "Q3", "Q4", NA, NA, NA, NA, NA, NA),
                           .Dim = c(3L, 3L), is.data.set.specified = TRUE))
})

test_that("parseInputTextIntoVariableNamesMatrix (variable name wildcard supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Wildcard character
    input.text <- "Q1*"
    v.name.matrix <- parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                           input.data.sets.metadata = input.data.sets.metadata,
                                                           allow.wildcards = TRUE,
                                                           input.purpose = "Variables to manually include")
    expect_equal(v.name.matrix,
                 structure(c("Q1A", NA, "Q1B"), .Dim = c(1L, 3L),
                           is.data.set.specified = FALSE))
})

test_that("parseInputTextIntoVariableNamesMatrix (error as wildcard not supported for variables to combine)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Error when wildcard character supplied when not allowed
    input.text <- "Q1*"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = FALSE,
                                                       input.purpose = "Variables to manually combine"),
                 paste0("The input 'Q1*' could not be parsed as wildcard ",
                        "characters are not supported for 'Variables to ",
                        "manually combine' inputs."), fixed = TRUE)
})

test_that("parseInputTextIntoVariableNamesMatrix (error as variable name not found)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Variable does not exist error
    input.text <- "Q99"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 paste0("The input variable 'Q99' could not be found in any ",
                        "of the input data sets. Ensure that the variable ",
                        "has been correctly specified."))
})

test_that("parseInputTextIntoVariableNamesMatrix (error as variable name not found)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # No wildcard match error
    input.text <- "Q99*"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 paste0("No variables matching the wildcard name 'Q99*' ",
                        "could be found in any of the input data sets. ",
                        "Ensure that the wildcard name has been correctly specified."),
                 fixed = TRUE)
})

test_that("parseInputTextIntoVariableNamesMatrix (error as data set index could not be parsed)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Data set index could not be parsed error
    input.text <- c("Q2(not an index)")
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 paste0("The data set indices in the input 'Q2(not an index)' ",
                        "could not be parsed. They need to be numbers ",
                        "corresponding to the data sets, e.g., 'Q2(3)'."),
                 fixed = TRUE)
})

test_that("parseInputTextIntoVariableNamesMatrix (error as data set index is out of range)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Data set index out of range error
    input.text <- "Q2(99)"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 paste0("The data set index in the input 'Q2(99)' is out of ",
                        "range. Data set indices must be between 1 and the ",
                        "number of input data sets (3)."), fixed = TRUE)
})

test_that("parseInputTextIntoVariableNamesMatrix (error as variable not found in sepcified data set)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Variable does not exist in specified data set error
    input.text <- "Q2(3)"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 paste0("The input variable 'Q2' could not be found in ",
                        "input data set 3. Ensure that the variable has been ",
                        "correctly specified."), fixed = TRUE)
})

test_that("parseInputTextIntoVariableNamesMatrix (error as no wildcard match found)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # No wildcard match in specified data set error
    input.text <- "Q1*(2)"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 "No variables were found in data set 2 matching the wildcard input 'Q1*'.",
                 fixed = TRUE)
})

test_that("parseInputTextIntoVariableNamesMatrix (error as wildcards not supported in variable ranges)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Wildcard in range error
    input.text <- "Q1*-Q4"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 paste0("The input 'Q1*-Q4' is invalid as wildcard characters ",
                        "are not supported for variable ranges."), fixed = TRUE)
})

test_that("parseInputTextIntoVariableNamesMatrix (error as variables in range not found)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Range not found error
    input.text <- "Q2-Q99"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 paste0("The input range 'Q2-Q99' was not found in any of ",
                        "the input data sets. Ensure that the range has been ",
                        "correctly specified."))
})

test_that("parseInputTextIntoVariableNamesMatrix (error as variables in range not found in specified data set)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Range not found in specified data set error
    input.text <- "Q1A(2)-Q4(2)"
    expect_error(parseInputTextIntoVariableNamesMatrix(input.text = input.text,
                                                       input.data.sets.metadata = input.data.sets.metadata,
                                                       allow.wildcards = TRUE,
                                                       input.purpose = "Variables to manually include"),
                 paste0("The input variable 'Q1A' could not be found in ",
                        "input data set 2. Ensure that the variable has been ",
                        "correctly specified."))
})

test_that("parseInputTextForVariableInteraction (variable names supplied)", {
    input.text <- "Q1A, Q1B"
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")),
                                     n.data.sets = 3)
    v.name.matrix <- parseInputTextForVariableInteraction(input.text = input.text,
                                                          input.data.sets.metadata = input.data.sets.metadata,
                                                          input.purpose = "Variables to manually combine")
    expect_equal(v.name.matrix,
                 structure(c("Q1A", NA, "Q1B"), .Dim = c(1L, 3L),
                           is.data.set.specified.vector = c(FALSE, FALSE, FALSE)))
})

test_that("parseInputTextForVariableInteraction (variable names with data set indices supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Data set indices specified
    input.text <- "Q3(2),Q3(3)"
    v.name.matrix <- parseInputTextForVariableInteraction(input.text = input.text,
                                                          input.data.sets.metadata = input.data.sets.metadata,
                                                          input.purpose = "Variables to manually combine")
    expect_equal(v.name.matrix,
                 structure(c(NA, "Q3", "Q3"), .Dim = c(1L, 3L),
                           is.data.set.specified.vector = c(FALSE, TRUE, TRUE)))
})

test_that("parseInputTextForVariableInteraction (variable range supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Variable range
    input.text <- "Q2-Q4"
    v.name.matrix <- parseInputTextForVariableInteraction(input.text = input.text,
                                                          input.data.sets.metadata = input.data.sets.metadata,
                                                          input.purpose = "Variables to manually combine")
    expect_equal(v.name.matrix,
                 structure(c("Q2", "Q3", "Q4", "Q2", "Q3", "Q4", NA, NA, NA),
                           .Dim = c(3L, 3L), is.data.set.specified.vector = c(FALSE, FALSE, FALSE)))
})

test_that("parseInputTextForVariableInteraction (variable range with data set indices supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Variable range with data set index
    input.text <- "Q2(1)-Q4(1),Q2(2)-Q4(2)"
    v.name.matrix <- parseInputTextForVariableInteraction(input.text = input.text,
                                                          input.data.sets.metadata = input.data.sets.metadata,
                                                          input.purpose = "Variables to manually combine")
    expect_equal(v.name.matrix,
                 structure(c("Q2", "Q3", "Q4", "Q2", "Q3", "Q4", NA, NA, NA),
                           .Dim = c(3L, 3L), is.data.set.specified.vector = c(TRUE, TRUE, FALSE)))
})

test_that("parseInputTextForVariableInteraction (error as only one variable supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Variables from only one data set error
    input.text <- "Q2(1)"
    expect_error(parseInputTextForVariableInteraction(input.text = input.text,
                                                      input.data.sets.metadata = input.data.sets.metadata,
                                                      input.purpose = "Variables to manually combine"),
                 paste0("The input 'Q2(1)' for 'Variables to manually combine' ",
                        "only specifies variables from one data set. ",
                        "It needs to specify variables from two or more data sets."),
                 fixed = TRUE)
})

test_that("parseInputTextForVariableInteraction (error as ranges with differing numbers of variables supplied)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    # Ranges with differing lengths error
    input.text <- "Q2(1)-Q3(1),Q2(2)-Q4(2)"
    expect_error(parseInputTextForVariableInteraction(input.text = input.text,
                                                      input.data.sets.metadata = input.data.sets.metadata,
                                                      input.purpose = "Variables to manually combine"),
                 paste0("The input 'Q2(1)-Q3(1),Q2(2)-Q4(2)' contains ",
                        "variable ranges with differing numbers of variables. ",
                        "Ensure that the ranges have been correctly specified ",
                        "so that they all contain the same number of variables."),
                 fixed = TRUE)
})

test_that("parseVariablesToCombine (variable names and ranges supplied)", {
    variables.to.combine <- c("Q1A, Q1B", "Q2-Q4")
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)
    v.names.to.combine <- parseVariablesToCombine(variables.to.combine = variables.to.combine,
                                                  input.data.sets.metadata = input.data.sets.metadata)
    expect_equal(v.names.to.combine,
                 structure(c("Q1A", "Q2", "Q3", "Q4", NA, "Q2", "Q3", "Q4", "Q1B",
                             NA, NA, NA),
                           .Dim = 4:3,
                           is.data.set.specified.matrix = structure(c(FALSE, FALSE, FALSE, FALSE,
                                                                      FALSE, FALSE, FALSE, FALSE,
                                                                      FALSE, FALSE, FALSE, FALSE), .Dim = 4:3)))
})

test_that("parseVariablesToCombine (error as variable specified multiple times)", {
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)

    variables.to.combine <- c("Q2", "Q2(1),Q3(2)")
    expect_error(parseVariablesToCombine(variables.to.combine = variables.to.combine,
                                         input.data.sets.metadata = input.data.sets.metadata),
                 paste0("The variable 'Q2' has been specified to be combined ",
                        "in multiple inputs: 'Q2', 'Q2(1),Q3(2)'. Ensure ",
                        "that any of the variables to be combined are ",
                        "specified in at most one input."), fixed = TRUE)
})

test_that("parseVariablesToNotCombine (variable names and ranges supplied)", {
    variables.to.not.combine <- c("Q1A, Q1B", "Q2-Q4")
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)
    v.names.to.not.combine <- parseVariablesToNotCombine(variables.to.not.combine = variables.to.not.combine,
                                                         input.data.sets.metadata = input.data.sets.metadata)
    expect_equal(v.names.to.not.combine,
                 structure(c("Q1A", "Q2", "Q3", "Q4", NA, "Q2", "Q3", "Q4", "Q1B",
                             NA, NA, NA),
                           .Dim = 4:3))
})

test_that("parseVariablesToKeep (variable names and ranges supplied)", {
    variables.to.keep <- c("Q1A, Q1B", "Q2-Q4")
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)
    v.names.to.keep <- parseVariablesToKeep(variables.to.keep = variables.to.keep,
                                            input.data.sets.metadata = input.data.sets.metadata)
    expect_equal(v.names.to.keep,
                 structure(c("Q1A", NA, "Q2", "Q3", "Q4", NA, NA, "Q2", "Q3",
                             "Q4", NA, "Q1B", NA, NA, NA), .Dim = c(5L, 3L)))
})

test_that("parseVariablesToOmit (variable names and ranges supplied)", {
    variables.to.omit <- c("Q1A, Q1B", "Q2-Q4")
    input.data.sets.metadata <- list(variable.names = list(c("Q1A", "Q2", "Q3", "Q4"),
                                                           c("Q2", "Q3", "Q4"),
                                                           c("Q1B", "Q3")), n.data.sets = 3)
    v.names.to.omit <- parseVariablesToOmit(variables.to.omit = variables.to.omit,
                                            input.data.sets.metadata = input.data.sets.metadata)
    expect_equal(v.names.to.omit,
                 structure(c("Q1A", NA, "Q2", "Q3", "Q4", NA, NA, "Q2", "Q3",
                             "Q4", NA, "Q1B", NA, NA, NA), .Dim = c(5L, 3L)))
})

test_that("matchPercentages (100% min.match.percentage)", {
    # 100% min.match.percentage, we set all non-matching strings to 0
    match.percentages <- matchPercentages(strings.1 = c("Coca-cola", "Pepsi"),
                                          strings.2 = c("Coca Kola", "Pepsi"),
                                          ignore.case = TRUE,
                                          ignore.non.alphanumeric = TRUE,
                                          min.match.percentage = 100)
    expect_equal(match.percentages,
                 structure(c(0, 0, 0, 100), .Dim = c(2L, 2L)))
})

test_that("matchPercentages (90% min.match.percentage)", {
    # 90% min.match.percentage
    match.percentages <- matchPercentages(strings.1 = c("Coca-cola", "Pepsi"),
                                          strings.2 = c("Coca Kola", "Pepsi"),
                                          ignore.case = TRUE,
                                          ignore.non.alphanumeric = TRUE,
                                          min.match.percentage = 90)
    expect_equal(match.percentages,
                 structure(c(87.499999999999, -Inf, -Inf, 100),
                           .Dim = c(2L, 2L)))
})

test_that("matchPercentages (90% min.match.percentage, don't ignore case)", {
    # Don't ignore case
    match.percentages <- matchPercentages(strings.1 = "Coca-cola",
                                          strings.2 = "Coca-Cola",
                                          ignore.case = FALSE,
                                          ignore.non.alphanumeric = TRUE,
                                          min.match.percentage = 90)
    expect_equal(match.percentages, matrix(87.5))
})

test_that("matchPercentages (90% min.match.percentage, don't ignore non-alphanumeric characters)", {
    # Don't ignore non-alphanumeric characters
    match.percentages <- matchPercentages(strings.1 = "Coca-cola",
                                          strings.2 = "Coca cola",
                                          ignore.case = TRUE,
                                          ignore.non.alphanumeric = FALSE,
                                          min.match.percentage = 90)
    expect_equal(match.percentages, matrix(88.8888888888879))
})

test_that("matchPercentagesForValueAttributes (80% min.match.percentage)", {
    val.attr.1 <- list(structure(1:3, .Names = c("Coca Cola", "Diet Coke", "Coke Zero")),
                       structure(1:3, .Names = c("Coke", "Diet Coke", "Coke Zero")),
                       structure(1:2, .Names = c("Pepsi", "Pepsi Max")))
    val.attr.2 <- list(structure(1:3, .Names = c("Coca-cola", "Diet Coke", "Coke Zero")))
    match.percentages <- matchPercentagesForValueAttributes(val.attrs.1 = val.attr.1,
                                                            val.attrs.2 = val.attr.2,
                                                            ignore.case = TRUE,
                                                            ignore.non.alphanumeric = TRUE,
                                                            min.match.percentage = 80)
    expect_equal(match.percentages,
                 structure(c(100, 76.9230769230759, -Inf), .Dim = c(3L, 1L)))
})

test_that("matchPercentagesForValueAttributes (80% min.match.percentage, don't ignore case)", {
    val.attr.1 <- list(structure(1:3, .Names = c("Coca Cola", "Diet Coke", "Coke Zero")),
                       structure(1:3, .Names = c("Coke", "Diet Coke", "Coke Zero")),
                       structure(1:2, .Names = c("Pepsi", "Pepsi Max")))
    val.attr.2 <- list(structure(1:3, .Names = c("Coca-cola", "Diet Coke", "Coke Zero")))

    # Don't ignore case
    match.percentages <- matchPercentagesForValueAttributes(val.attrs.1 = val.attr.1[1],
                                                            val.attrs.2 = val.attr.2,
                                                            ignore.case = FALSE,
                                                            ignore.non.alphanumeric = TRUE,
                                                            min.match.percentage = 80)
    expect_equal(match.percentages,
                 structure(98.2967307143484, .Dim = c(1L, 1L)))
})

test_that("matchPercentagesForValueAttributes (80% min.match.percentage, don't ignore non-alphanumeric characters)", {
    val.attr.1 <- list(structure(1:3, .Names = c("Coca Cola", "Diet Coke", "Coke Zero")),
                       structure(1:3, .Names = c("Coke", "Diet Coke", "Coke Zero")),
                       structure(1:2, .Names = c("Pepsi", "Pepsi Max")))
    val.attr.2 <- list(structure(1:3, .Names = c("Coca-cola", "Diet Coke", "Coke Zero")))

    # Don't ignore non-alphanumeric characters
    match.percentages <- matchPercentagesForValueAttributes(val.attrs.1 = val.attr.1[1],
                                                            val.attrs.2 = val.attr.2,
                                                            ignore.case = TRUE,
                                                            ignore.non.alphanumeric = FALSE,
                                                            min.match.percentage = 80)
    expect_equal(match.percentages,
                 structure(99.1033090162442, .Dim = c(1L, 1L)))
})

test_that("adjustedMatchPercentage", {
    # All these have the same match percentage of 80%
    adjusted <- adjustedMatchPercentage(distances = c(2, 4, 10, 20),
                                        max.nchars = c(10, 20, 50, 100))
    # Adjusted match percentages get closer to 100% when max.nchars is large
    expect_equal(adjusted,
                 c(79.999999999999, 79.999999999999,
                   95.999999999999, 99.839999999999))
})

test_that("removeNonAlphaNumericCharacters", {
    expect_equal("Q1_23A", removeNonAlphaNumericCharacters(txt = "Q1__23__A#"))
})

test_that("isNumbersPreserved", {
    expect_false(isNumbersPreserved(string.1 = "Q23_4", string.2 = "Q234"))
    expect_true(isNumbersPreserved(string.1 = "Q23_4", string.2 = "Q__23__4"))
})

test_that("isVariableCombinableIntoRow (variable not able to be combined)", {
    v.names.to.not.combine <- matrix(c(NA_character_, "Q1B", "Q1C"), ncol = 3)
    expect_false(isVariableCombinableIntoRow(name.to.combine = "Q1B",
                                             data.set.ind = 2,
                                             matched.names.row = c("Q1A", NA_character_, "Q1C"),
                                             v.names.to.not.combine = v.names.to.not.combine))
})

test_that("isVariableCombinableIntoRow (variable able to be combined)", {
    v.names.to.not.combine <- matrix(c(NA_character_, "Q1B", "Q1D"), ncol = 3)
    expect_true(isVariableCombinableIntoRow(name.to.combine = "Q1B",
                                            data.set.ind = 2,
                                            matched.names.row = c("Q1A", NA_character_, "Q1C"),
                                            v.names.to.not.combine = v.names.to.not.combine))
})

test_that("isMissingValue", {
    expect_equal(isMissingValue(text = c(NA, "", "na", "n/a", "-", "not missing")),
                 c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
})

test_that("isParsableAsNumeric (character vector able to be parsed)", {
    expect_true(isParsableAsNumeric(text = c(NA, "", "na", "n/a", "-", "123")))
})

test_that("isParsableAsNumeric (character vector not able to be parsed)", {
    expect_false(isParsableAsNumeric(text = c(NA, "", "na", "n/a", "-", "not numeric",
                                       "123")))
})

test_that("isParsableAsDateTime", {
    expect_true(isParsableAsDateTime(text = c(NA, "", "na", "n/a", "-", "7 June 2021")))
    expect_false(isParsableAsDateTime(text = c(NA, "", "na", "n/a", "-", "not a date",
                                               "7 June 2021")))
})

test_that("isConvertibleToDateTime", {
    expect_true(isConvertibleToDateTime(num = as.numeric(AsDateTime("7 Jun 2021"))))
    expect_false(isConvertibleToDateTime(num = 2021))
})

test_that("mergedVariableNames (preferring first data set)", {
    matched.names <- matrix(c("Q1", NA_character_, "Q3", "Q1B", "Q2", "Q3B"),
                            ncol = 2)
    merged.names <- mergedVariableNames(matched.names = matched.names,
                                        use.names.and.labels.from = "First data set")
    expect_equal(merged.names,
                 structure(c("Q1", "Q2", "Q3"),
                           renamed.variables = structure(logical(0), .Dim = c(0L, 2L),
                                                         .Dimnames = list(NULL, c("Original name", "New name")))))
})

test_that("mergedVariableNames (preferring last data set)", {
    matched.names <- matrix(c("Q1", NA_character_, "Q3", "Q1B", "Q2", "Q3B"),
                            ncol = 2)
    merged.names <- mergedVariableNames(matched.names = matched.names,
                                        use.names.and.labels.from = "Last data set")
    expect_equal(merged.names,
                 structure(c("Q1B", "Q2", "Q3B"),
                           renamed.variables = structure(logical(0), .Dim = c(0L, 2L),
                                                         .Dimnames = list(NULL, c("Original name", "New name")))))
})

test_that("mergedVariableNames (variable renamed due to name conflict)", {
    # Q1 appears in 2 rows so the second one gets renamed
    matched.names <- matrix(c("Q1", NA_character_, NA_character_, "Q3",
                              NA_character_, "Q1", "Q2", "Q3B"), ncol = 2)
    merged.names <- mergedVariableNames(matched.names = matched.names,
                                        use.names.and.labels.from = "First data set")
    expect_equal(merged.names,
                 structure(c("Q1", "Q1_1", "Q2", "Q3"),
                           renamed.variables = structure(c("Q1", "Q1_1"), .Dim = 1:2,
                                                         .Dimnames = list(NULL, c("Original name", "New name")))))
})

test_that("mergeIndicesList (example with 8 indices)", {
    merged.indices <- mergeIndicesList(indices.list = list(c(1L, 2L, 4L, 6L, 7L, 8L),
                                                           c(3L, 5L, 6L, 8L),
                                                           c(2L, 3L, 4L, 5L)),
                                       prefer.first.element = TRUE)
    expect_equal(merged.indices, 1:8)
})

test_that("mergeIndicesList (all possible orders of name vectors in list)", {
    names.list <- list(c(1L, 2L, 4L),
                       c(1L, 3L, 4L),
                       c(2L, 4L, 5L),
                       c(1L, 2L, 3L))

    perm <- permutations(4, 4)
    for (i in seq_len(nrow(perm)))
        expect_equal(mergeIndicesList(indices.list = names.list[perm[i, ]],
                                      prefer.first.element = TRUE), 1:5)
})

test_that("mergeIndicesList (example with 11 indices)", {
    merged.indices <- mergeIndicesList(indices.list = list(c(1L, 4L, 5L, 6L, 7L, 8L),
                                                           c(2L, 3L, 5L, 9L),
                                                           c(3L, 4L, 5L, 10L, 11L)),
                                       prefer.first.element = TRUE)
    expect_equal(merged.indices, 1:11)
})

test_that("mergeIndicesList (indices to keep together specified)", {
    # indices.to.keep.together
    merged.indices <- mergeIndicesList(indices.list = list(c(1L, 2L, 4L, 6L, 7L, 8L),
                                                           c(3L, 5L, 6L, 8L),
                                                           c(2L, 3L, 4L, 5L)),
                                       prefer.first.element = TRUE,
                                       indices.to.keep.togther = list(c(1L, 8L),
                                                                      c(2L, 7L)))
    expect_equal(merged.indices, c(1L, 8L, 2L, 7L, 3L:6L))
})
