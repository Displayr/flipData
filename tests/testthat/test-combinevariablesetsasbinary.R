library(testthat)
library(verbs)
context("Combine variable sets as binary")


data(phone, package = "flipExampleData")
data(colas, package = "flipExampleData")


phone.names <- c("AAPT", "New Tel", "One-tel", "Optus", "Orange", "Telstra", "Virgin", "Vodafone", "Other 1", "Other 2", "Don't know")
unaided <- data.frame(phone[, paste0("Q5_", 1:11)])
colnames(unaided) <- phone.names
unaided[1, ] <- NA
unaided = (unaided == "Yes") * 1
attr(unaided, "questiontype") <- "PickAny"
attr(unaided, "dataset") <- "phone"

aided = data.frame(phone[, paste0("Q6_", 1:11)])
colnames(aided) <- phone.names
aided = (aided == "Yes") * 1
attr(aided, "questiontype") <- "PickAny"
attr(aided, "dataset") <- "phone"

Q4 = as.data.frame(colas[, c("q4a", "q4b", "q4c", "q4d", "q4e", "q4f")])
attr(Q4, "questiontype") <- "PickOneMulti"
attr(Q4, "dataset") <- "colas"

Q4.small = as.data.frame(colas[, c("q4a", "q4b", "q4c", "q4d")])
attr(Q4.small, "questiontype") <- "PickOneMulti"
attr(Q4.small, "dataset") <- "colas"

Q4.pepsi.light = colas[, "q4e"]
attr(Q4.pepsi.light, "questiontype") <- "PickOne"
attr(Q4.pepsi.light, "dataset") <- "colas"

Q4.pepsi.max = colas[, "q4f"]
attr(Q4.pepsi.max, "questiontype") <- "PickOne"
attr(Q4.pepsi.max, "dataset") <- "colas"

Q4.binary = CombineVariableSetsAsBinary(Q4)
Q4.binary = cbind(Q4.binary, "NET" = rep(TRUE, nrow(Q4.binary)))
attr(Q4.binary, "codeframe") <- list("Hate" = 1, "Dislike" = 2, "Neither like not dislike" = 3, "Love" = 4, "Like" = 5, "NET" = c(1,2,3,4,5))
attr(Q4.binary, "questiontype") <- "PickAny"
attr(Q4.binary, "dataset") <- "colas"

Q4.binary.small = CombineVariableSetsAsBinary(Q4)
attr(Q4.binary.small, "questiontype") <- "PickAny"
attr(Q4.binary.small, "dataset") <- "colas"

test_that("Single PickOne", {

    asnumeric = flipTransformations::AsNumeric(Q4.pepsi.light, binary = TRUE)
    colnames(asnumeric) = levels(Q4.pepsi.light)
    asnumeric = asnumeric == 1

    expect_equal(CombineVariableSetsAsBinary(Q4.pepsi.light), asnumeric)

})

test_that("Two PickOnes", {

    pepsi.light.numeric = flipTransformations::AsNumeric(Q4.pepsi.light, binary = TRUE)
    colnames(pepsi.light.numeric) = levels(Q4.pepsi.light)
    pepsi.max.numeric = flipTransformations::AsNumeric(Q4.pepsi.max, binary = TRUE)
    colnames(pepsi.max.numeric) = levels(Q4.pepsi.max)
    input.args = list(pepsi.light.numeric, pepsi.max.numeric)
    input.args[["match.elements"]] <- "Yes"
    input.args[["elements.to.count"]] <- list(numeric = 1, categorical = NULL)
    input.args[["ignore.missing"]] <- TRUE
    pepsi.light.or.max = do.call(AnyOf, input.args)

    expect_equal(CombineVariableSetsAsBinary(Q4.pepsi.light, Q4.pepsi.max), pepsi.light.or.max)
})

test_that("Many PickOnes are equivalent to a PickOneMulti", {

    expect_equal(CombineVariableSetsAsBinary(Q4), 
        CombineVariableSetsAsBinary(colas[, "q4a"], colas[, "q4b"], colas[, "q4c"], colas[, "q4d"], colas[, "q4e"], colas[, "q4f"]))

})

test_that("Combining PickOnes and Pick Any", {

    expect_equal(Q4.binary[, -ncol(Q4.binary)], CombineVariableSetsAsBinary(Q4.binary.small, Q4.pepsi.light, Q4.pepsi.max), check.attributes = FALSE)

})

test_that("Pick Any returns same data", {

    expect_equal(CombineVariableSetsAsBinary(Q4.binary), Q4.binary[, -ncol(Q4.binary)], check.attributes = FALSE)
    expect_equal(CombineVariableSetsAsBinary(Q4.binary, Q4.binary), Q4.binary[, -ncol(Q4.binary)], check.attributes = FALSE)

})


test_that("Missing data", {

    input.args <- list(aided, unaided)
    input.args[["match.elements"]] <- "Yes"
    input.args[["elements.to.count"]] <- list(numeric = NA, categorical = NULL)
    input.args[["ignore.missing"]] <- TRUE

    n.missing <- do.call(Count, input.args)
    expect_true(all(is.na(CombineVariableSetsAsBinary(aided, unaided)[n.missing == 2])))
    expect_true(all(is.na(CombineVariableSetsAsBinary(aided, unaided, compute.for.incomplete = FALSE)[n.missing > 0])))
})

test_that("Filling in unmatched columns correctly", {

    aided.2 <- aided
    attr(aided.2, "originalquestiontype") <- "Pick Any"
    expect_equal(fillInCategoriesWhenNotPresent(aided.2, colnames(aided.2)), aided.2)
    expect_true(all(is.na(fillInCategoriesWhenNotPresent(aided.2, c(colnames(aided.2), "Hello"))[, "Hello"])))

    Q4.pepsi.light.2 <- Q4.pepsi.light
    Q4.pepsi.light.2[c(1,2,3)] <- NA
    Q4.pepsi.light.binary <- CombineVariableSetsAsBinary(Q4.pepsi.light.2)
    attr(Q4.pepsi.light.binary, "originalquestiontype") <- "Pick One"

    expect_equal(which(is.na(fillInCategoriesWhenNotPresent(Q4.pepsi.light.binary, c(colnames(Q4.pepsi.light.binary), "Hello"))[, "Hello"])), c(1,2,3))

})

test_that("Unmatched columns included", {
    aided.2 <- aided
    colnames(aided.2)[11] <- "Hello"
    combined <- CombineVariableSetsAsBinary(aided.2, unaided)
    unique.cols <- unique(c(colnames(aided.2), colnames(unaided)))
    expect_true(all(colnames(combined) %in% unique.cols))
    expect_true(all(unique.cols %in% colnames(combined)))
    expect_equal(as.numeric(combined[, "Hello"]), aided.2[, "Hello"])
})



test_that("Error messages", {
    aided.2 <- aided
    colnames(aided.2)[11] <- "Telstra"
    expect_error(CombineVariableSetsAsBinary(aided.2, unaided), "duplicate")
})
