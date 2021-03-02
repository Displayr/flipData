context("MergeDataSetsByCase")

test_that("mergeNamesRespectingOrder", {
    merged.names <- mergeNamesRespectingOrder(list(c("a", "b", "d", "f", "g", "h"),
                                                   c("c", "e", "f", "h"),
                                                   c("b", "c", "d", "e")))
    expect_equal(merged.names, letters[1:8])
})

test_that("matchNamesExactly", {
    matched.names <- matchNamesExactly(list(c("a", "b", "c"), c("a", "c", "d")))
    expect_equal(matched.names,
                 structure(c("a", "b", "c", NA, "a", NA, "c", "d"),
                           .Dim = c(4L, 2L), .Dimnames = list(c("a", "b", "c", "d"), NULL)))
})


