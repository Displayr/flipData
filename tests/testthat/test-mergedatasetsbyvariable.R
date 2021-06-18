context("MergeDataSetsByVariable")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

# cola15.sav: Q1_*, Q2, Q3, Q3_3, Attr1 included, with "High self-monitor" cases removed.
# cola16.sav: Q2, Q3, Q3_3, Q4_A, Q4_B, Q4_C, Attr1 (renamed as PartyID) included,
#             with "Low self-monitor" cases removed.

test_that("Example used for widget test in flipFormat", {
    expect_error(merge.data.set.by.var.output <- MergeDataSetsByVariable(data.set.names = c(findInstDirFile("cola15.sav"),
                                                                                            findInstDirFile("cola16.sav")),
                                                                         id.variables = "Attr1,PartyID",
                                                                         variables.to.include.or.omit = list("Q1_A_c", "Q4_B")), NA)
})
