context("Stacking")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

test_that("stacking", {
    StackData(findInstDirFile("Cola.sav"),
              common.labels = c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                                "Diet Pepsi", "Pepsi Max", "None of these"))
})
