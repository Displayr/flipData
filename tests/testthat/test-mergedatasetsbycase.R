context("MergeDataSetsByCase")

test_that("merge names", {
    merged.names <- mergeNames(list(c("a", "b", "d", "f", "g", "h"),
                                    c("c", "e", "f", "h"),
                                    c("b", "c", "d", "e")))
    expect_equal(merged.names, letters[1:8])
})
