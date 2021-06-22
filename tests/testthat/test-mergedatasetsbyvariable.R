context("MergeDataSetsByVariable")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

# cola15.sav: Q1_*, Q2, Q3, Q3_3, Attr1 included, with "High self-monitor" cases removed.
# cola16.sav: Q2, Q3, Q3_3, Q4_A, Q4_B, Q4_C, Attr1 (renamed as PartyID) included,
#             with "Low self-monitor" cases removed.
# cola17.sav: Q1_* to Q4_* included, with non-"High self-monitor" cases removed.
# cola18.sav: Q1_* to Q4_* included, with IDs in Attr1 modified to include duplicate values.

test_that("Example used for widget test in flipFormat", {
    expect_error(merge.data.set.by.var.output <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                                                            findInstDirFile("cola16.sav")),
                                                                         id.variables = c("Attr1","PartyID"),
                                                                         variables.to.include.or.omit = list("Q1_A_c", "Q4_B")), NA)
})

test_that("No ID variables", {
    # Combine data sets side-by-side, they need to have the same number of cases
    result <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                         findInstDirFile("cola15.sav")),
                                      include.merged.data.set.in.output = TRUE)
    expect_equal(length(result$merged.data.set), 20)
    expect_equal(names(result$merged.data.set)[11:20],
                 paste0(names(result$merged.data.set)[1:10], "_1"))
    expect_true(all(result$merged.data.set[1] == result$merged.data.set[11]))

    expect_error(MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                            findInstDirFile("cola16.sav"))),
                 paste0("The data sets could not be combined without ID variables ",
                        "(side-by-side, no matching) as they have differing numbers ",
                        "of cases. To combine them, ID variables need to be specified."),
                 fixed = TRUE)
})

test_that("Match by ID variables", {
    result <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                         findInstDirFile("cola16.sav")),
                                      id.variables = c("Attr1","PartyID"),
                                      include.merged.data.set.in.output = TRUE)
    expect_equal(result$merged.id.variable.name, "Attr1")
    expect_true("Attr1" %in% names(result$merged.data.set))
    expect_false("PartyID" %in% names(result$merged.data.set))

    # Check IDs from first ID variable
    input.data.sets <- readDataSets(c(findInstDirFile("cola15.sav"),
                                      findInstDirFile("cola16.sav")))
    expect_true(all(result$merged.data.set[["Attr1"]][1:251] ==
                    input.data.sets$cola15.sav[["Attr1"]]))
    expect_true(all(result$merged.data.set[["Q2"]][1:251] ==
                        input.data.sets$cola15.sav[["Q2"]]))
    expect_true(all(is.na(result$merged.data.set[["Q2"]][252:327])))

    # Check IDs from second ID variable
    remaining.ids <- setdiff(input.data.sets$cola16.sav[["PartyID"]],
                             input.data.sets$cola15.sav[["Attr1"]])
    expect_true(all(result$merged.data.set[["Attr1"]][252:327] == remaining.ids))
    remaining.ind <- match(remaining.ids, input.data.sets$cola16.sav[["PartyID"]])
    expect_true(all(result$merged.data.set[["Q2_1"]][252:327] == input.data.sets$cola16.sav[["Q2_1"]]))

    # Check ID variables are appropriate
    expect_error(MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                            findInstDirFile("cola16.sav")),
                                         id.variables = c("Q3","Q3"),
                                         include.merged.data.set.in.output = TRUE),
                 paste0("The data sets cannot be merged by the specified ID ",
                        "variables as the ID '3' is duplicated in multiple ",
                        "data sets."))

    # Check ID variables have common values
    expect_warning(MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                            findInstDirFile("cola17.sav")),
                                         id.variables = c("Attr1","Attr1"),
                                         include.merged.data.set.in.output = TRUE),
                   paste0("There are no common IDs between the two input data sets. ",
                          "Ensure that the ID variable names have been correctly specified."))
})

test_that("Include and omit variables", {
    # Specify variables to omit from both data sets
    result <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                         findInstDirFile("cola16.sav")),
                                      id.variables = c("Attr1","PartyID"),
                                      include.or.omit.variables = c("Include all variables except those manually omitted",
                                                                    "Include all variables except those manually omitted"),
                                      variables.to.include.or.omit = list("Q1_A_c", "Q4_B"), # omitted
                                      include.merged.data.set.in.output = TRUE)
    expect_false("Q1_A_c" %in% names(result$merged.data.set))
    expect_false("Q4_B" %in% names(result$merged.data.set))
    expect_equal(names(result$merged.data.set),
                 c("Q1_F_c", "Q1_E_c1", "Q1_D_c", "Q1_C_c1", "Q1_B_c1", "Q2",
                   "Q3", "Q3_3", "Attr1", "Q2_1", "Q3_1", "Q3_3_1", "Q4_A", "Q4_C"))

    # Specify variables to omit from the first data set,
    # specify variables to include from the second data set
    result <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                         findInstDirFile("cola16.sav")),
                                      id.variables = c("Attr1","PartyID"),
                                      include.or.omit.variables = c("Include all variables except those manually omitted",
                                                                    "Only include manually specified variables"),
                                      variables.to.include.or.omit = list("Q2", "Q3"),
                                      include.merged.data.set.in.output = TRUE)
    expect_false("Q2" %in% names(result$merged.data.set))
    expect_true("Q3" %in% names(result$merged.data.set))
    expect_equal(names(result$merged.data.set),
                 c(c("Q1_F_c", "Q1_E_c1", "Q1_D_c", "Q1_C_c1", "Q1_A_c",
                     "Q1_B_c1", "Q3", "Q3_3", "Attr1", "Q3_1")))
})

test_that("Only keep cases matched to all data sets", {
    result <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                         findInstDirFile("cola16.sav")),
                                      id.variables = c("Attr1","PartyID"),
                                      only.keep.cases.matched.to.all.data.sets = TRUE,
                                      include.merged.data.set.in.output = TRUE)
    input.data.sets <- readDataSets(c(findInstDirFile("cola15.sav"),
                                      findInstDirFile("cola16.sav")))
    common.ids <- intersect(input.data.sets$cola15.sav[["Attr1"]],
                            input.data.sets$cola16.sav[["PartyID"]])
    expect_true(all(result$merged.data.set[["Attr1"]] == common.ids))
})

test_that("mergedIDVariableType", {
    expect_equal(mergedIDVariableType(c("Numeric", "Numeric")), "Numeric")
    expect_equal(mergedIDVariableType(c("Numeric", "Categorical")), "Text")
    expect_equal(mergedIDVariableType(c("Date", "Date/Time")), "Date/Time")
})

test_that("convertIDVariableType", {
    expect_equal(convertIDVariableType(1:10, "Numeric", "Text"),
                 as.character(1:10))

    date.id <- structure(c(18799, 18800, 18801), class = "Date")
    expect_equal(convertIDVariableType(date.id, "Date", "Date/Time"),
                 structure(c(1624233600, 1624320000, 1624406400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"))

    categorical.id <- c(1,2,1)
    attr(categorical.id, "labels") <- structure(1:2, .Names = c("A", "B"))
    expect_equal(convertIDVariableType(categorical.id, "Categorical", "Text"),
                 c("A", "B", "A"))
})

test_that("matchCasesWithoutIDVariables", {
    input.data.sets.metadata <- list(n.data.sets = 3, n.cases = 10)
    expect_equal(matchCasesWithoutIDVariables(input.data.sets.metadata),
                 matrix(rep(1:10, 3), ncol = 3))
})

test_that("orderVariablesUsingInputDataSet", {
    expect_equal(orderVariablesUsingInputDataSet(c("T", "W", "A"), LETTERS),
                 c("A", "T", "W"))
})

test_that("parseInputVariableTextForDataSet", {
    expect_equal(parseInputVariableTextForDataSet(c("A,B, C", "X-", "G-J"), LETTERS),
                 c("A", "B", "C", "X", "Y", "Z", "G", "H", "I", "J"))
})

test_that("exampleIDValues", {
    categorical.id <- c(1,2,1)
    attr(categorical.id, "labels") <- structure(1:2, .Names = c("A", "B"))

    data.sets <- list(data.frame(list(ID_1 = 1:10, VAR_1 = 11:20)),
                      data.frame(list(ID_2 = 21:30, VAR_2 = 31:40)),
                      data.frame(list(ID_3 = categorical.id)))
    expect_equal(exampleIDValues(c("ID_1", "ID_2", "ID_3"), data.sets),
                 c("1", "21", "A"))
})
