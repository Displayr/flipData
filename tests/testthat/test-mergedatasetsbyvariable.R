context("MergeDataSetsByVariable")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

# cola15.sav: Q1_*, Q2, Q3, Q3_3, Attr1 included, with "High self-monitor" cases removed.
# cola16.sav: Q2, Q3, Q3_3, Q4_A, Q4_B, Q4_C, Attr1 (renamed as PartyID) included,
#             with "Low self-monitor" cases removed.
# cola17.sav: Q1_* to Q4_* included, with only "High self-monitor" cases kept.
# cola18.sav: Q1_* to Q4_* included, with IDs in Attr1 (renamed as Attr1_dup)
#             modified to include duplicate values.

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
})

test_that("Error when no ID variables supplied and data sets have different number of cases", {
    expect_error(MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                            findInstDirFile("cola16.sav"))),
                 paste0("The data sets could not be combined without ID variables ",
                        "(side-by-side, no matching) as they have differing numbers ",
                        "of cases. To combine them, ID variables need to be specified."),
                 fixed = TRUE)
})

test_that("Match by ID variables (no duplicates)", {
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
})

test_that("Match by ID variables (IDs duplicated in one data set)", {
    # IDs duplicated in (cola18.sav)
    input.data.sets <- readDataSets(c(findInstDirFile("cola17.sav"),
                                      findInstDirFile("cola18.sav")))
    result <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola17.sav"),
                                                         findInstDirFile("cola18.sav")),
                                      id.variables = c("Attr1","Attr1_dup"),
                                      include.merged.data.set.in.output = TRUE)

    # First 3 cases are from a duplicated ID (appears 3 times)
    expect_true(all(result$merged.data.set$Attr1[1:3] == "2066969"))

    # Values are duplicated for variables from the data set without duplicate IDs
    expect_true(all(result$merged.data.set$Q3[1:3] == input.data.sets$cola17.sav$Q3[1]))

    # Values are not duplicated for variables from the data set with duplicate IDs
    expect_equal(result$merged.data.set$Q3_1[1:3], c(5, 2, 8))

    # IDs in the data set with duplicate IDs are duplicated (appear 3 times)
    dup.ids <- input.data.sets$cola18.sav$Attr1_dup
    expect_true(all(sapply(dup.ids, function(id) sum(result$merged.data.set$Attr1 == id)) == 3))

    # IDs not in the data set with duplicate IDs are not duplicated
    non.dup.ids <- setdiff(input.data.sets$cola17.sav$Attr1,
                           input.data.sets$cola18.sav$Attr1_dup)
    expect_true(all(sapply(non.dup.ids, function(id) sum(result$merged.data.set$Attr1 == id)) == 1))
})

test_that("Error when matching by ID variables and an ID is duplicated in multiple data sets", {
    expect_error(MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                            findInstDirFile("cola16.sav")),
                                         id.variables = c("Q3","Q3"),
                                         include.merged.data.set.in.output = TRUE),
                 paste0("The data sets cannot be merged by the specified ID ",
                        "variables as the ID '3' is duplicated in multiple ",
                        "data sets."))
})

test_that("Warning when matching by ID variables and data sets have no IDs in common", {
    expect_warning(MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                              findInstDirFile("cola17.sav")),
                                           id.variables = c("Attr1","Attr1"),
                                           include.merged.data.set.in.output = TRUE),
                   paste0("There are no common IDs between the two input data sets. ",
                          "Ensure that the ID variable names have been correctly specified."))
})

test_that("Specify variables to omit from both data sets", {
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
})

test_that("Specify variables to omit using wildcard", {
    result <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                         findInstDirFile("cola16.sav")),
                                      id.variables = c("Attr1","PartyID"),
                                      include.or.omit.variables = c("Include all variables except those manually omitted",
                                                                    "Include all variables except those manually omitted"),
                                      variables.to.include.or.omit = list("Q1_*", ""), # omitted
                                      include.merged.data.set.in.output = TRUE)
    expect_true(all(!grepl("Q1_*", names(result$merged.data.set))))

})

test_that("Specify variables to omit from first data set, variables to include from second data set", {
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
    expect_equal(mergedIDVariableType(id.variable.types = c("Numeric", "Numeric")), "Numeric")
    expect_equal(mergedIDVariableType(id.variable.types = c("Numeric", "Categorical")), "Text")
    expect_equal(mergedIDVariableType(id.variable.types = c("Date", "Date/Time")), "Date/Time")
})

test_that("convertIDVariableType", {
    expect_equal(convertIDVariableType(ids = 1:10,
                                       id.variable.type = "Numeric",
                                       merged.id.variable.type = "Text"),
                 as.character(1:10))

    date.id <- structure(c(18799, 18800, 18801), class = "Date")
    expect_equal(convertIDVariableType(ids = date.id,
                                       id.variable.type = "Date",
                                       merged.id.variable.type = "Date/Time"),
                 structure(c(1624233600, 1624320000, 1624406400),
                           class = c("POSIXct", "POSIXt"), tzone = "UTC"))

    categorical.id <- c(1,2,1)
    attr(categorical.id, "labels") <- structure(1:2, .Names = c("A", "B"))
    expect_equal(convertIDVariableType(ids = categorical.id,
                                       id.variable.type = "Categorical",
                                       merged.id.variable.type = "Text"),
                 c("A", "B", "A"))
})

test_that("matchCasesWithoutIDVariables", {
    input.data.sets.metadata <- list(n.data.sets = 3, n.cases = 10)
    expect_equal(matchCasesWithoutIDVariables(input.data.sets.metadata = input.data.sets.metadata),
                 matrix(rep(1:10, 3), ncol = 3))
})

test_that("orderVariablesUsingInputDataSet", {
    expect_equal(orderVariablesUsingInputDataSet(var.names.to.order = c("T", "W", "A"),
                                                 data.set.var.names = LETTERS),
                 c("A", "T", "W"))
})

test_that("parseInputVariableTextForDataSet", {
    expect_equal(parseInputVariableTextForDataSet(input.text = c("A,B, C", "X-", "G-J"),
                                                  data.set.variable.names = LETTERS,
                                                  data.set.index = 1),
                 c("A", "B", "C", "X", "Y", "Z", "G", "H", "I", "J"))
})

test_that("parseInputVariableTextForDataSet with wildcard", {
    expect_equal(parseInputVariableTextForDataSet(input.text = c("Coke*"),
                                                  data.set.variable.names = c("Coca-cola", "Coke Zero",
                                                                              "Diet Coke", "Coke No Sugar"),
                                                  data.set.index = 1),
                 c("Coke Zero", "Coke No Sugar"))
})

test_that("parseInputVariableTextForDataSet error when wildcard supplied in range", {
    expect_error(parseInputVariableTextForDataSet(input.text = c("Coke*-Pepsi"),
                                                  data.set.variable.names = c("Coca-cola", "Coke Zero",
                                                                              "Diet Coke", "Coke No Sugar"),
                                                  data.set.index = 1),
                 paste0("The input 'Coke*-Pepsi' is invalid as wildcard ",
                        "characters are not supported for variable ranges."),
                 fixed = TRUE)
})

test_that("exampleIDValues", {
    categorical.id <- c(1,2,1)
    attr(categorical.id, "labels") <- structure(1:2, .Names = c("A", "B"))

    data.sets <- list(data.frame(list(ID_1 = 1:10, VAR_1 = 11:20)),
                      data.frame(list(ID_2 = 21:30, VAR_2 = 31:40)),
                      data.frame(list(ID_3 = categorical.id)))
    expect_equal(exampleIDValues(id.variable.names = c("ID_1", "ID_2", "ID_3"),
                                 data.sets = data.sets),
                 c("1", "21", "A"))
})
