library(testthat)
library(verbs)
context("Combine variable sets as binary")


data(phone, package = "flipExampleData")
data(colas, package = "flipExampleData")


phone.names <- c("AAPT", "New Tel", "One-tel", "Optus", "Orange", "Telstra",
                 "Virgin", "Vodafone", "Other 1", "Other 2", "Don't know")
unaided <- data.frame(phone[, paste0("Q5_", 1:11)])
colnames(unaided) <- phone.names
unaided[1, ] <- NA
unaided <- (unaided == "Yes") * 1
attr(unaided, "questiontype") <- "PickAny"
attr(unaided, "dataset") <- "phone"

aided <- data.frame(phone[, paste0("Q6_", 1:11)])
colnames(aided) <- phone.names
aided <- (aided == "Yes") * 1
attr(aided, "questiontype") <- "PickAny"
attr(aided, "dataset") <- "phone"

q4 <- as.data.frame(colas[, c("q4a", "q4b", "q4c", "q4d", "q4e", "q4f")])
attr(q4, "questiontype") <- "PickOneMulti"
attr(q4, "dataset") <- "colas"

q4.small <- as.data.frame(colas[, c("q4a", "q4b", "q4c", "q4d")])
attr(q4.small, "questiontype") <- "PickOneMulti"
attr(q4.small, "dataset") <- "colas"

q4.pepsi.light <- colas[, "q4e"]
attr(q4.pepsi.light, "questiontype") <- "PickOne"
attr(q4.pepsi.light, "dataset") <- "colas"

q4.pepsi.max <- colas[, "q4f"]
attr(q4.pepsi.max, "questiontype") <- "PickOne"
attr(q4.pepsi.max, "dataset") <- "colas"

q4.binary <- CombineVariableSetsAsBinary(q4)
q4.binary <- cbind(q4.binary, "NET" = rep(TRUE, nrow(q4.binary)))
attr(q4.binary, "codeframe") <- list(
    "Hate" = 1, "Dislike" = 2, "Neither like not dislike" = 3, "Love" = 4, "Like" = 5,
    "NET" = 1:5
)
attr(q4.binary, "questiontype") <- "PickAny"
attr(q4.binary, "dataset") <- "colas"

q4.binary.small <- CombineVariableSetsAsBinary(q4)
attr(q4.binary.small, "questiontype") <- "PickAny"
attr(q4.binary.small, "dataset") <- "colas"

test_that("Single PickOne", {

    asnumeric <- flipTransformations::AsNumeric(q4.pepsi.light, binary = TRUE)
    colnames(asnumeric) <- levels(q4.pepsi.light)
    asnumeric <- asnumeric == 1

    expect_equal(CombineVariableSetsAsBinary(q4.pepsi.light), asnumeric)

})

test_that("Two PickOnes", {

    pepsi.light.numeric <- flipTransformations::AsNumeric(q4.pepsi.light, binary = TRUE)
    colnames(pepsi.light.numeric) <- levels(q4.pepsi.light)
    pepsi.max.numeric <- flipTransformations::AsNumeric(q4.pepsi.max, binary = TRUE)
    colnames(pepsi.max.numeric) <- levels(q4.pepsi.max)
    input.args <- list(pepsi.light.numeric, pepsi.max.numeric)
    input.args[["match.elements"]] <- "Yes"
    input.args[["elements.to.count"]] <- list(numeric = 1, categorical = NULL)
    input.args[["ignore.missing"]] <- TRUE
    pepsi.light.or.max <- do.call(AnyOf, input.args)

    expect_equal(CombineVariableSetsAsBinary(q4.pepsi.light, q4.pepsi.max), pepsi.light.or.max)
})

test_that("Many PickOnes are equivalent to a PickOneMulti", {

    expect_equal(
        CombineVariableSetsAsBinary(q4),
        CombineVariableSetsAsBinary(colas$q4a, colas$q4b, colas$q4c, colas$q4d, colas$q4e, colas$q4f)
    )

})

test_that("Multliple PickOneMulti where one is the subset of the other", {
    expect_equal(CombineVariableSetsAsBinary(q4, q4.small), CombineVariableSetsAsBinary(q4))
})

test_that("Combining PickOnes and Pick Any", {

    expect_equal(
        q4.binary[, -ncol(q4.binary)],
        CombineVariableSetsAsBinary(q4.binary.small, q4.pepsi.light, q4.pepsi.max), check.attributes = FALSE
    )

})

test_that("Pick Any returns same data", {

    expect_equal(CombineVariableSetsAsBinary(q4.binary), q4.binary[, -ncol(q4.binary)], check.attributes = FALSE)
    expect_equal(CombineVariableSetsAsBinary(q4.binary, q4.binary), q4.binary[, -ncol(q4.binary)], check.attributes = FALSE)

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

    q4.pepsi.light.2 <- q4.pepsi.light
    q4.pepsi.light.2[1:3] <- NA
    q4.pepsi.light.binary <- CombineVariableSetsAsBinary(q4.pepsi.light.2)
    attr(q4.pepsi.light.binary, "originalquestiontype") <- "Pick One"

    expect_equal(which(is.na(fillInCategoriesWhenNotPresent(q4.pepsi.light.binary, c(colnames(q4.pepsi.light.binary), "Hello"))[, "Hello"])), c(1,2,3))

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

    test.case.1 <- factor(c("", "A", "B", "C", "A", "B", "C"))
    test.case.2 <- factor(c("A", "B", "C", "A", "B", "C"), levels = c("", "A", "B", "C"))
    expect_error(CombineVariableSetsAsBinary(test.case.1, test.case.2), "cases")
})

test_that("Blank factor labels", {
    test.case.1 <- factor(c("", "A", "B", "C", "A", "B", "C"))
    test.case.2 <- factor(c("A", "A", "B", "C", "A", "B", "C"), levels = c("", "A", "B", "C"))

    expect_equal(colnames(CombineVariableSetsAsBinary(test.case.1, test.case.2)), c("", "A", "B", "C"))
})

test_that("Large variable sets aren't slow", {
    x <- replicate(
        2L,
        factor(sample(c(NA, letters[1:3]), size = 1e4, replace = TRUE)),
        simplify = FALSE
    ) |>
        as.data.frame() |>
        structure(questiontype = "PickOneMulti")
    y <- replicate(
        2L,
        factor(sample(c(NA, letters[1:3]), size = 1e4, replace = TRUE)),
        simplify = FALSE
    ) |>
        as.data.frame() |>
        structure(questiontype = "PickOneMulti")

    (CombineVariableSetsAsBinary(x, y) |>
         system.time())["elapsed"] |>
        expect_lt(1)
    # Rownames checking works
    x.small <- x[1:10, ] |> structure(questiontype = "PickOneMulti")
    y.small <- y[1:10, ] |> structure(questiontype = "PickOneMulti")
    CombineVariableSetsAsBinary(x.small, y.small) |> expect_silent()
    # Happy with single variables
    single.var <- factor(sample(c(NA, letters[1:3]), size = 10, replace = TRUE)) |>
        structure(questiontype = "PickOne")
    CombineVariableSetsAsBinary(single.var, x.small) |> expect_silent()
    # Check rownames with error if inconsistent
    y.offset <- y[11:20, ] |> structure(questiontype = "PickOneMulti")
    CombineVariableSetsAsBinary(x.small, y.offset) |>
        expect_error(
            paste0(
                "The input variables do not have the same number of cases, ",
                "please select variables from the same data set"
            )
        )
    assign("productName", "Displayr", envir = .GlobalEnv)
    on.exit(rm("productName", envir = .GlobalEnv))
    CombineVariableSetsAsBinary(x.small, y.offset) |>
        expect_error(
            paste0(
                "The input variables do not have the same number of cases, ",
                "please select variables from the same data source"
            )
        )
    CombineVariableSetsAsBinary(single.var, y.offset) |>
        expect_error(
            paste0(
                "The input variables do not have the same number of cases, ",
                "please select variables from the same data source"
            )
        )
})
