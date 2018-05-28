context("TidyRawData")

data("cola", package = "flipExampleData")
x <- cola[, c("Q6_A", "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F")]

test_that("TidyRawData: warns when converting factors to numeric",
          {
                expect_warning(TidyRawData(x, as.numeric = TRUE))

          })

wgt = c(NA, 0, rep(1, 100), rep(NA, 225))


################################################################################
## Bug fixes
################################################################################
test_that("DS-1644: TidyRawData: provides a warning if a duplicate variable is removed.",
          {
            data(colas, package = "flipExampleData")
            z = colas[, c(1:5)]
            nms <- c("A", "A", "B", "A", "B")
            names(z) = nms
            expect_warning(TidyRawData(z),
                "Variables containing duplicated variable names have been removed (give the variables unique names if you do not want this to happen): A, B.",
                fixed = TRUE)
            expect_equal(suppressWarnings(names(TidyRawData(z))), c("A", "B"))

            df <- data.frame(x = 1:2, x = 3:4, check.names = FALSE)
            expect_equal(suppressWarnings(names(TidyRawData(df))), "x")
          })



################################################################################
## non-data.fame inputs
################################################################################

test_that("TidyRawData: error if NULL input",
          {
              expect_error(TidyRawData(NULL))
          })

test_that("TidyRawData: single character vector input",
          {
              out <- rep(c("a", "b"), times = 5)
              expect_silent(out <- TidyRawData(out))
              expect_equal(class(out), "data.frame")
              expect_equal(class(out[[1L]]), "factor")
              expect_equal(levels(out[[1L]]), c("a", "b"))
          })

test_that("TidyRawData: single factor input",
          {
              out <- as.factor(rep(c("a", "b"), times = 5))
              expect_silent(out <- TidyRawData(out))
              expect_equal(class(out), "data.frame")
              expect_equal(class(out[[1L]]), "factor")
              expect_equal(levels(out[[1L]]), c("a", "b"))

          })

################################################################################
## labels
################################################################################

original.labels = flipFormat::Labels(x)

test_that("TidyRawData: formatted labels are correct",
    {
        out <- (TidyRawData(x, as.numeric = FALSE))
        expect_equal(original.labels, flipFormat::Labels(out))
        out <- (TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = FALSE))
        expect_equal(original.labels, flipFormat::Labels(out))
        out <- suppressWarnings(TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = TRUE))
        expect_equal(original.labels, flipFormat::Labels(out))
    })

test_that("TidyRawData: extracts label prefix and simplifies labels",
    {
        short.lab <- sub("^Q6[.] ", "", original.labels)
        out <- suppressWarnings(TidyRawData(x, as.numeric = FALSE,
                                            extract.common.lab.prefix = TRUE))
        expect_equal(flipFormat::Labels(out), short.lab)
        expect_equal(attr(out, "label.prefix"), "Q6.", check.attributes = FALSE)

        ## Old labels preserved if shortening not requested
        out <- suppressWarnings(TidyRawData(x, as.numeric = FALSE,
                                            extract.common.lab.prefix = FALSE))
        expect_equal(flipFormat::Labels(out), original.labels, check.attributes = FALSE)

        ## Okay with as.binary = as.numeric = TRUE creating extra vars
        blabs <- as.vector(sapply(x, function(col) paste0(Labels(col), ": ", levels(col))))
        out <- suppressWarnings(TidyRawData(x, as.numeric = TRUE, as.binary = TRUE,
                                            extract.common.lab.prefix = FALSE))
        expect_equal(flipFormat::Labels(out), blabs, check.attributes = FALSE)
    })

################################################################################
## subset
################################################################################

test_that("TidyRawData: fails when weights or subset wrong length",
    {
        # Checking mismatched weight.
        wgt2 = 0:327
        expect_error(TidyRawData(x, weights = wgt2))

        # Checking mismatched subset
        expect_error(TidyRawData(x, subset = wgt2 > 5))
    })

test_that("TidyRawData: subset deals with NAs appropriable",
    {
        expect_equal((nrow(TidyRawData(x, subset = wgt > .5))), 100)
        expect_equal((nrow(TidyRawData(x, subset = wgt >= 0))), 101)
        out <- (TidyRawData(x, weights = wgt,  subset = wgt > .5))
        expect_equal(nrow(out), 100)
        # factor
        expect_true(is.factor(out[,1]))
        # numeric
        out <- suppressWarnings(TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = TRUE))
        expect_true(!is.factor(out[,1]))
        # binary
        out <- suppressWarnings(TidyRawData(x, weights = wgt,  subset = wgt > .5, as.numeric = TRUE, as.binary = TRUE))
        expect_equal(ncol(out), 36)
    })

test_that("TidyRawData: warns if subsetting removes a factor level",
    {
        idx <- x$Q6_C != "Hate"
        expect_warning(TidyRawData(x, subset = idx))
    })

################################################################################
## weights
################################################################################

out <- suppressWarnings(TidyRawData(x, weights = wgt))
test_that("TidyRawData:  missing values in weights cause data to be filtered",
    {
        expect_equal(nrow(out), 100)
        expect_equivalent(out, x[!is.na(wgt) & wgt, ])  # expect_equivalent(out, x[3:102, ])
    })

################################################################################
## Imputation
################################################################################

x1 <- data.frame(x = 1:100, y = 100:1)
x1$x[seq(10, nrow(x1), by = 10)] <- NA

test_that("TidyRawData: single imputation",
          {
              out <- TidyRawData(x1, missing = "Exclude cases with missing data")
              expect_equal(nrow(out), nrow(x1) - sum(is.na(x1$x)))
          })


test_that("TidyRawData: single imputation and subset",
          {
              subset <- logical(100)
              subset[1:50] <- TRUE
              out <- TidyRawData(x1, subset = subset,
                                 missing = "Imputation (replace missing values with estimates)")
              expect_equal(nrow(out), sum(subset))
              expect_false(any(is.na(out)))
          })


test_that("TidyRawData: error if remove so much NA data that more variables than data left",
          {
              x1$x[1:99] <- NA
              expect_error(TidyRawData(x1, missing = "Exclude cases with missing data"))
          })


################################################################################
## Coercing type
################################################################################

z <- data.frame(numeric = as.character(1:100),
                character = factor(paste0(1:100,LETTERS[1:10])),
                factor = LETTERS[1:4],
                POSIXct = "2017/01/01",
                logical = c("T", "T", "F", "F", "na"))

test_that("TidyRawData: auto.correct.class works",
          {
              out <- TidyRawData(z, auto.correct.class = TRUE)
              cls <- lapply(out, "class")
              expect_equal(cls$numeric, "numeric")
              expect_equal(cls$character, "character")
              expect_equal(cls$factor, "factor")
              expect_equal(cls$POSIXct, c("POSIXct", "POSIXt"))
              expect_equal(cls$logical, "logical")
          })

