context("TidyRawData")

test_that("TidyRawData",
          {
                data("cola", package = "flipExampleData")
                x <- cola[,c("Q6_A", "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F")]

                expect_error(suppressWarnings(TidyRawData(x, as.numeric = TRUE)), NA)
                # Checking mismatched weight.
                wgt = 0:327
                expect_error(TidyRawData(x, weights = wgt))
                # Checking mismatched subset
                expect_error(TidyRawData(x, subset = wgt > 5))
                # Checking that missing values in weights cause data to be filtered
                wgt = c(NA, 0, rep(1, 100), rep(NA, 225))
                expect_equal(suppressWarnings(nrow(TidyRawData(x, weights = wgt))), 100)
                # Checking subset deals with NAs appropriable
                expect_equal(suppressWarnings(nrow(TidyRawData(x, subset = wgt > .5))), 100)
                expect_equal(suppressWarnings(nrow(TidyRawData(x, subset = wgt >= 0))), 101)
                x1 <- suppressWarnings(TidyRawData(x, weights = wgt,  subset = wgt > .5))
                expect_equal(nrow(x1), 100)
                # factor
                expect_true(is.factor(x1[,1]))
                # numeric
                x1 <- suppressWarnings(TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = TRUE))
                expect_true(!is.factor(x1[,1]))
                # binary
                x1 <- suppressWarnings(TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = TRUE, as.binary = TRUE))
                expect_equal(ncol(x1), 36)

          })

