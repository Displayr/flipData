context("MergeDataSetsByCase")

# need tests of numeric variables!
# add test for prioritize.early.data.sets

library(gtools)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

test_that("mergeIndicesList", {
    merged.indices <- mergeIndicesList(list(c(1L, 2L, 4L, 6L, 7L, 8L),
                                            c(3L, 5L, 6L, 8L),
                                            c(2L, 3L, 4L, 5L)),
                                            prioritize.early.elements = TRUE)
    expect_equal(merged.indices, 1:8)

    names.list <- list(c(1L, 2L, 4L),
                       c(1L, 3L, 4L),
                       c(2L, 4L, 5L),
                       c(1L, 2L, 3L))

    perm <- permutations(4,4)
    for (i in seq_len(nrow(perm)))
        expect_equal(mergeIndicesList(names.list[perm[i, ]],
                                      prioritize.early.elements = TRUE), 1:5)

    merged.indices <- mergeIndicesList(list(c(1L, 4L, 5L, 6L, 7L, 8L),
                                            c(2L, 3L, 5L, 9L),
                                            c(3L, 4L, 5L, 10L, 11L)),
                                       prioritize.early.elements = TRUE)
    expect_equal(merged.indices, 1:11)
})

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

test_that("merge cola data, exact match by variable names", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola2.sav"),
                                                              findInstDirFile("cola3.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           write.data.set = FALSE)

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
    expect_equal(unique(merged.data.set$Q2[328:981]), c(2,1)) # Retain original values
    expect_equal(attr(merged.data.set$Q2, "labels"), c(M = 1, F = 2)) # Use labels from earlier data set

    # values in Q3_3 of the 2nd data set are modified (keeping the same labels)
    # but upon merging, the values in the last data set are used
    expect_true(all(merged.data.set$Q3_3[328:654] == merged.data.set$Q3_3[655:981]))
    expect_equal(attr(merged.data.set$Q3_3, "labels"),
                 c(`Less than 18` = 18, `18 to 24` = 22, `25 to 29` = 27, `30 to 34` = 33,
                   `35 to 39` = 37, `40 to 44` = 42, `45 to 49` = 47, `50 to 54` = 52,
                   `55 to 64` = 60, `65 or more` = 70))

    expect_equal(result$merge.map$input.names,
                 structure(c("Q1_F_c", "Q1_E_c1", "Q1_D_c", "Q1_C_c1", "Q1_A_c",
                             "Q1_B_c1", NA, "Q3", "Q4_A", "Q4_B", "Q4_C", "Q4_A_3", "Q4_B_2",
                             "Q4_C_2", "Q3_3", "Q4_A_5", "Q4_B_3", "Q4_C_3", "Q4_A_6", "Q4_B_4",
                             "Q4_C_4", NA, "Q1_F_c", "Q1_E_c1", "Q1_D_c", "Q1_C_c1", "Q1_A_c",
                             "Q1_B_c1", "Q2", "Q3", "Q4_A", "Q4_B", "Q4_C", "Q4_A_3", "Q4_B_2",
                             "Q4_C_2", "Q3_3", "Q4_A_5", "Q4_B_3", "Q4_C_3", "Q4_A_6", "Q4_B_4",
                             "Q4_C_4", NA, NA, NA, NA, NA, NA, NA, "Q2", "Q3", "Q4_A", "Q4_B",
                             "Q4_C", "Q4_A_3", "Q4_B_2", "Q4_C_2", "Q3_3", NA, NA, NA, NA,
                             NA, NA, "Q1_F"), .Dim = c(22L, 3L)))
    expect_equal(result$merge.map$merged.names,
                 c("Q1_F_c", "Q1_E_c1", "Q1_D_c", "Q1_C_c1", "Q1_A_c", "Q1_B_c1",
                   "Q2", "Q3", "Q4_A", "Q4_B", "Q4_C", "Q4_A_3", "Q4_B_2", "Q4_C_2",
                   "Q3_3", "Q4_A_5", "Q4_B_3", "Q4_C_3", "Q4_A_6", "Q4_B_4", "Q4_C_4",
                   "Q1_F"))
    expect_equal(result$merge.map$non.combinable.variables,
                 structure(logical(0), .Dim = c(0L, 3L)))
    expect_equal(result$merge.map$renamed.variables, list())
})


test_that("create new values when a single categorical value has multiple labels", {
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola2.sav"),
                                                              findInstDirFile("cola3.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           write.data.set = FALSE,
                                           category.value.with.multiple.labels = "Create new values")$merged.data.set

    expect_true(all(is.na(merged.data.set$Q2[1:327]))) # 1st data set does not have Q2
    expect_equal(unique(merged.data.set$Q2[328:654]), c(2,1)) # original values for gender
    expect_equal(unique(merged.data.set$Q2[655:981]), c(4,3)) # new values for gender
    expect_equal(attr(merged.data.set$Q2, "labels"), c(M = 1, F = 2, Male = 3, Female = 4))
})

test_that("variables to combine", {
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola4.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           variables.to.combine = "Q3_3,Q3_3_new_name",
                                           write.data.set = FALSE)$merged.data.set
    expect_true("Q3_3" %in% names(merged.data.set))
    expect_false("Q3_3_new_name" %in% names(merged.data.set))
    expect_true(all(!is.na(merged.data.set$Q3_3)))

    # Data set index
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola4.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           variables.to.combine = "Q3_3_new_name(2),Q3_3(1)",
                                           write.data.set = FALSE)$merged.data.set
    expect_true("Q3_3" %in% names(merged.data.set))
    expect_false("Q3_3_new_name" %in% names(merged.data.set))
    expect_true(all(!is.na(merged.data.set$Q3_3)))

    # Range
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola5.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           variables.to.combine = "Q4_A_3 - Q4_C_2,Q4_A_3_new-Q4_C_2_new",
                                           write.data.set = FALSE)$merged.data.set
    expect_true(all(c("Q4_A_3", "Q4_B_2", "Q4_C_2") %in% names(merged.data.set)))
    expect_false(any(c("Q4_A_3_new", "Q4_B_2_new", "Q4_C_2_new") %in% names(merged.data.set)))
    expect_true(all(!is.na(merged.data.set$Q4_A_3_new)))
    expect_true(all(!is.na(merged.data.set$Q4_B_2_new)))
    expect_true(all(!is.na(merged.data.set$Q4_C_2_new)))

    # Range with data set indices
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola5.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           variables.to.combine = "Q4_A_3(1) - Q4_C_2,Q4_A_3_new(2)-Q4_C_2_new(2)",
                                           write.data.set = FALSE)$merged.data.set
    expect_true(all(c("Q4_A_3", "Q4_B_2", "Q4_C_2") %in% names(merged.data.set)))
    expect_false(any(c("Q4_A_3_new", "Q4_B_2_new", "Q4_C_2_new") %in% names(merged.data.set)))
    expect_true(all(!is.na(merged.data.set$Q4_A_3_new)))
    expect_true(all(!is.na(merged.data.set$Q4_B_2_new)))
    expect_true(all(!is.na(merged.data.set$Q4_C_2_new)))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                           findInstDirFile("cola5.sav")),
                        match.by = "Variable names",
                        include.merged.data.set.in.output = TRUE,
                        variables.to.combine = "Q4_A_3 - Q4_B_2,Q4_A_3_new-Q4_C_2_new",
                        write.data.set = FALSE),
                 paste0("The input 'Q4_A_3 - Q4_B_2,Q4_A_3_new-Q4_C_2_new' ",
                        "contains variable ranges with differing numbers of variables. ",
                        "Ensure that the ranges have been correctly specified so that ",
                        "they all contain the same number of variables."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     variables.to.combine = "Q3_3",
                                     write.data.set = FALSE),
                 "The input 'Q3_3' is invalid as it needs to contain two or more variables.")

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     variables.to.combine = "Q3_3,Q3_3_new_name",
                                     variables.to.omit = "Q3_3",
                                     write.data.set = FALSE),
                 paste0("The variable 'Q3_3' has been specified to be both combined and omitted. ",
                        "Ensure that it is specified to be either combined or or omitted."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     variables.to.combine = c("Q3_3,Q3_3_new_name", "Q3_3,Q2"),
                                     write.data.set = FALSE),
                 paste0("The variable 'Q3_3' has been specified to be combined in multiple inputs: ",
                        "'Q3_3,Q3_3_new_name', 'Q3_3,Q2'. Ensure that any of ",
                        "the variables to be combined are specified in at most one input."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     variables.to.combine = "Q4_3,Q3_3_new_name",
                                     write.data.set = FALSE),
                 paste0("The input variable 'Q4_3' could not be found in any ",
                        "of the input data sets. Ensure that the variable ",
                        "has been correctly specified."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     variables.to.combine = "Q2-Q4,Q2_new-Q4_new",
                                     write.data.set = FALSE),
                 paste0("The input range 'Q2-Q4' was not found in any of ",
                        "the input data sets. Ensure that the range has ",
                        "been correctly specified."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     variables.to.combine = "Q2-Q3-Q4,Q2_new-Q3_new-Q4_new",
                                     write.data.set = FALSE),
                 paste0("The input range 'Q2-Q3-Q4' was not found in any of ",
                        "the input data sets. Ensure that the range has been ",
                        "correctly specified."), fixed = TRUE)
})

test_that("variables to not combine", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                     findInstDirFile("cola2.sav"),
                                                     findInstDirFile("cola3.sav")),
                                  match.by = "Variable names",
                                  write.data.set = FALSE,
                                  variables.to.not.combine = "Q3")
    expect_true(all(c("Q3", "Q3_1", "Q3_2") %in% result$merged.data.set.metadata$variable.names))
})

test_that("omit variables", {
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola2.sav"),
                                                              findInstDirFile("cola3.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           variables.to.omit = c("Q2(2)", "Q1_F(3)"),
                                           write.data.set = FALSE)$merged.data.set

    expect_true(all(is.na(merged.data.set$Q2[1:654])))
    expect_true(all(!is.na(merged.data.set$Q2[655:981])))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola2.sav")),
                                     match.by = "Variable names",
                                     variables.to.omit = "bad_var",
                                     write.data.set = FALSE),
                 paste0("The input variable 'bad_var' could not be found in ",
                        "any of the input data sets. Ensure that the ",
                        "variable has been correctly specified."),
                 fixed = TRUE)
})

test_that("error messages", {
    expect_error(MergeDataSetsByCase(data.set.names = findInstDirFile("cola1.sav"),
                                     write.data.set = FALSE),
                 "At least 2 data set(s) are required.", fixed = TRUE)
})

test_that("Variable type conversion", {
    # Q2 (gender) is categorical in cola2 and numeric in cola6
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola2.sav"),
                                                              findInstDirFile("cola6.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           write.data.set = FALSE)
    expect_equal(unname(merged.data.set$merged.data.set.metadata$variable.types["Q2"]),
                 "Categorical")
    expect_true(!any(is.na(merged.data.set$merged.data.set$Q2)))
})

test_that("Non-combinable variables", {
    result <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola3.sav"),
                                                     findInstDirFile("cola7.sav")),
                                  match.by = "Variable names",
                                  write.data.set = FALSE)
    # Q3_3 in the two datasets cannot be combined as the latter is a text
    # variable with many different values. So a new variable Q3_3_1 is created
    # immediately below Q3_3
    expect_true(all(result$merged.data.set.metadata$variable.names[9:10] == c("Q3_3", "Q3_3_1")))
    expect_equal(result$merge.map$non.combinable.variables,
                 structure(c("Q3_3", "Q3_3"), .Dim = 1:2))
    expect_equal(result$merge.map$renamed.variables,
                 list(list(original.name = "Q3_3", new.name = "Q3_3_1")))
})
