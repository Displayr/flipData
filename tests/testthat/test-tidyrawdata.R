context("TidyRawData")

data("cola", package = "flipExampleData")
x <- cola[, c("Q6_A", "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F")]

test_that("TidyRawData: warns when converting factors to numeric",
          {
                expect_warning(TidyRawData(x, as.numeric = TRUE))

          })


test_that("TidyRawData: fails when weights or subset wrong length",
    {
        # Checking mismatched weight.
        wgt = 0:327
        expect_error(TidyRawData(x, weights = wgt))

        # Checking mismatched subset
        expect_error(TidyRawData(x, subset = wgt > 5))
    })

test_that("TidyRawData:  missing values in weights cause data to be filtered",
    {
        wgt = c(NA, 0, rep(1, 100), rep(NA, 225))
        out <- suppressWarnings(TidyRawData(x, weights = wgt))
        expect_equal(nrow(out), 100)
        expect_equivalent(out, x[!is.na(wgt) & wgt, ])  # expect_equivalent(out, x[3:102, ])
    })

test_that("TidyRawData: subset deals with NAs appropriable",
    {
        expect_equal((nrow(TidyRawData(x, subset = wgt > .5))), 100)
        expect_equal((nrow(TidyRawData(x, subset = wgt >= 0))), 101)
        x1 <- (TidyRawData(x, weights = wgt,  subset = wgt > .5))
        expect_equal(nrow(x1), 100)
        # factor
        expect_true(is.factor(x1[,1]))
        # numeric
        x1 <- (TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = TRUE))
        expect_true(!is.factor(x1[,1]))
        # binary
        x1 <- (TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = TRUE, as.binary = TRUE))
        expect_equal(ncol(x1), 36)
    })

test_that("TidyRawData: warns if subsetting removes a factor level",
    {
        idx <- x$Q6_C != "Hate"
        expect_warning(TidyRawData(x, subset = idx))
    })


test_that("TidyRawData: formatted labels are correct",
    {
        original.labels = flipFormat::Labels(x)
        x1 <- (TidyRawData(x, as.numeric = FALSE))
        expect_equal(original.labels, Labels(x1))
        x1 <- (TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = FALSE))
        expect_equal(original.labels, Labels(x1))
        x1 <- (TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = TRUE))
        expect_equal(original.labels, Labels(x1))


    })

test_that("TidyRawData: extracts label prefix",
    {
        colnames(x) <- sub("_", ": ", colnames(x))
        x1 <- suppressWarnings(TidyRawData(x, as.numeric = FALSE))
        expect_equal(colnames(x1), LETTERS[1:6])
        expect_equal(attr(x1, "label.prefix"), "Q6")
    })


test_that("TidyRawData: NULL input",
          {

          })

test_that("TidyRawData: single numeric input",
          {


          })

test_that("TidyRawData: single factor input",
          {

          })

test_that("TidyRawData: returns common label prefix",
          {

          })

test_that("TidyRawData: returns label prefix",
          {

          })

test_that("TidyRawData: missing",
          {

          })

test_that("TidyRawData: imputation label",
          {

          })

test_that("TidyRawData: weight label",
          {

          })
