context("MergeDataSetsByCase")

# need tests of numeric variables!

library(gtools)

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

test_that("mergeNamesListRespectingOrder", {
    merged.names <- mergeNamesListRespectingOrder(list(c("a", "b", "d", "f", "g", "h"),
                                                   c("c", "e", "f", "h"),
                                                   c("b", "c", "d", "e")))
    expect_equal(merged.names, letters[1:8])

    names.list <- list(c("a", "b", "d"),
                       c("a", "c", "d"),
                       c("b", "d", "e"),
                       c("a", "b", "c"))

    perm <- permutations(4,4)
    for (i in seq_len(nrow(perm)))
        expect_equal(mergeNamesListRespectingOrder(names.list[perm[i, ]]), letters[1:5])
})

test_that("matchNamesExactly", {
    matched.names <- matchNamesExactly(list(c("a", "b", "c"), c("a", "c", "d")))
    expect_equal(matched.names,
                 structure(c("a", "b", "c", NA, "a", NA, "c", "d"),
                           .Dim = c(4L, 2L), .Dimnames = list(c("a", "b", "c", "d"), NULL)))
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

test_that("merge cola data, exact match by variable names", {
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola2.sav"),
                                                              findInstDirFile("cola3.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           write.data.set = FALSE)$merged.data.set
    expect_equivalent(merged.data.set$mergesrc, rep(1:3, each = 327))
    expect_equal(attr(merged.data.set$mergesrc, "label"), "Source of cases")
    expect_equal(names(merged.data.set), c("Q1_F_c", "Q1_E_c1", "Q1_D_c", "Q1_C_c1", "Q1_A_c", "Q1_B_c1",
                                           "Q2", "Q3", "Q3_3", "Q4_A", "Q4_B", "Q4_C", "Q4_A_3", "Q4_B_2",
                                           "Q4_C_2", "Q1_F", "Q4_A_5", "Q4_B_3", "Q4_C_3", "Q4_A_6", "Q4_B_4",
                                           "Q4_C_4", "mergesrc"))
    expect_equal(vapply(merged.data.set, attr, character(1), "label"),
                 c(Q1_F_c = "Coke", Q1_E_c1 = "Diet Coke", Q1_D_c = "Coke Zero",
                   Q1_C_c1 = "Pepsi", Q1_A_c = "Diet Pepsi", Q1_B_c1 = "Pepsi Max",
                   Q2 = "Q2. Gender", Q3 = "Q3. Age", Q3_3 = "Q3. Age in years",
                   Q4_A = "Colas (e.g., Coca Cola, Pepsi Max)?", Q4_B = "Sparkling mineral water",
                   Q4_C = "Coffee", Q4_A_3 = "Colas (e.g., Coca Cola, Pepsi Max)?",
                   Q4_B_2 = "Sparkling mineral water", Q4_C_2 = "Coffee", Q1_F = "Q1.  Fragments - Coke",
                   Q4_A_5 = "Colas (e.g., Coca Cola, Pepsi Max)?", Q4_B_3 = "Sparkling mineral water",
                   Q4_C_3 = "Coffee", Q4_A_6 = "Q4.  Frequency numeric: Colas (e.g., Coca Cola, Pepsi Max)?",
                   Q4_B_4 = "Q4.  Frequency numeric: Sparkling mineral water", Q4_C_4 = "Q4.  Frequency numeric: Coffee",
                   mergesrc = "Source of cases"))

    expect_true(all(is.na(merged.data.set$Q2[1:327]))) # 1st data set does not have Q2
    expect_equal(unique(merged.data.set$Q2[328:654]), c(4,3)) # Q2 in 2nd data set has labels "M", "F", hence new values created
    expect_equal(unique(merged.data.set$Q2[655:981]), c(2,1)) # 3rd data set has original Q2
    expect_equal(attr(merged.data.set$Q2, "labels"), c(Male = 1, Female = 2, M = 3, F = 4))

    # values in Q3_3 of the 2nd data set are modified (keeping the same labels)
    # but upon merging, the values in the last data set are used
    expect_true(all(merged.data.set$Q3_3[328:654] == merged.data.set$Q3_3[655:981]))
    expect_equal(attr(merged.data.set$Q3_3, "labels"),
                 c(`Less than 18` = 18, `18 to 24` = 22, `25 to 29` = 27, `30 to 34` = 33,
                   `35 to 39` = 37, `40 to 44` = 42, `45 to 49` = 47, `50 to 54` = 52,
                   `55 to 64` = 60, `65 or more` = 70))
})

test_that("manual matches", {
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola4.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           manual.matches = "Q3_3,Q3_3_new_name",
                                           write.data.set = FALSE)$merged.data.set
    expect_true(!("Q3_3" %in% names(merged.data.set)))
    expect_true("Q3_3_new_name" %in% names(merged.data.set))
    expect_true(all(!is.na(merged.data.set$Q3_3_new_name)))

    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola5.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           manual.matches = "Q4_A_3 - Q4_C_2,Q4_A_3_new-Q4_C_2_new",
                                           write.data.set = FALSE)$merged.data.set
    expect_true(all(!(c("Q4_A_3", "Q4_B_2", "Q4_C_2") %in% names(merged.data.set))))
    expect_true(all(c("Q4_A_3_new", "Q4_B_2_new", "Q4_C_2_new") %in% names(merged.data.set)))
    expect_true(all(!is.na(merged.data.set$Q4_A_3_new)))
    expect_true(all(!is.na(merged.data.set$Q4_B_2_new)))
    expect_true(all(!is.na(merged.data.set$Q4_C_2_new)))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                           findInstDirFile("cola5.sav")),
                        match.by = "Variable names",
                        include.merged.data.set.in.output = TRUE,
                        manual.matches = "Q4_A_3 - Q4_B_2,Q4_A_3_new-Q4_C_2_new",
                        write.data.set = FALSE),
                 paste0("The manual match input 'Q4_A_3 - Q4_B_2,Q4_A_3_new-Q4_C_2_new' ",
                        "contains variable ranges with differing numbers of variables. ",
                        "Ensure that the ranges have been correctly specified so that ",
                        "they all contain the same number of variables."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     manual.matches = "Q3_3",
                                     write.data.set = FALSE),
                 "The manual match input 'Q3_3' is invalid as it needs to contain two or more variables.")

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     manual.matches = "Q3_3,Q3_3_new_name",
                                     variables.to.omit = list("Q3_3", character()),
                                     write.data.set = FALSE),
                 paste0("The variable Q3_3 has been specified in both a manual match and to be omitted. ",
                        "It needs to be removed from the manual match or the variables to be omitted."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     manual.matches = c("Q3_3,Q3_3_new_name", "Q3_3,Q2"),
                                     write.data.set = FALSE),
                 paste0("The variable 'Q3_3' has been specified in multiple manual match inputs: ",
                        "'Q3_3,Q3_3_new_name', 'Q3_3,Q2'. Ensure that all variables are only ",
                        "specified in at most one manual match."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     manual.matches = "Q4_3,Q3_3_new_name",
                                     write.data.set = FALSE),
                 paste0("The variable 'Q4_3' could not be found in the data set 'cola1.sav'."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     manual.matches = "Q2-Q4,Q2_new-Q4_new",
                                     write.data.set = FALSE),
                 paste0("The variable 'Q2' from the input range 'Q2-Q4' ",
                        "could not be found in the data set 'cola1.sav'."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola4.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     manual.matches = "Q2-Q3-Q4,Q2_new-Q3_new-Q4_new",
                                     write.data.set = FALSE),
                 paste0("The input range 'Q2-Q3-Q4' could not be recognized. ",
                        "It needs to contain the start and end variable names ",
                        "separated by a dash (-)."), fixed = TRUE)
})

test_that("omit variables", {
    merged.data.set <- MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola2.sav"),
                                                              findInstDirFile("cola3.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           variables.to.omit = list(character(),
                                                                    "Q2",
                                                                    "Q1_F"),
                                           write.data.set = FALSE)$merged.data.set

    expect_true(all(is.na(merged.data.set$Q2[1:654])))
    expect_true(all(!is.na(merged.data.set$Q2[655:981])))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                              findInstDirFile("cola2.sav")),
                                           match.by = "Variable names",
                                           include.merged.data.set.in.output = TRUE,
                                           variables.to.omit = list("just_one"),
                                           write.data.set = FALSE),
                 paste0("variables.to.omit must be specified as a list whose ",
                        "elements are the variables to omit in the ",
                        "corresponding data sets."))

    expect_error(MergeDataSetsByCase(data.set.names = c(findInstDirFile("cola1.sav"),
                                                        findInstDirFile("cola2.sav")),
                                     match.by = "Variable names",
                                     include.merged.data.set.in.output = TRUE,
                                     variables.to.omit = list("bad_var", character(0)),
                                     write.data.set = FALSE),
                 paste0("The following variable(s) were specified to be ",
                        "omitted but could not be found in the data set ",
                        "cola1.sav: bad_var."), fixed = TRUE)
})


test_that("error messages", {
    expect_error(MergeDataSetsByCase(data.set.names = findInstDirFile("cola1.sav"),
                                     write.data.set = FALSE),
                 "Merging requires at least two data sets.")
})
