context("AutoCoerceClass")

library(lubridate)
test_that("AutoCoerceClass",
          {
                # Dates - salesforce format
                z <- c('2018-05-14T17:06:09.000Z','2018-05-14T17:06:09.000Z','2018-05-14T16:35:38.000Z','2018-05-14T17:06:09.000Z',NA,'2018-05-14T17:06:09.000Z')
                expect_true(is.POSIXt(AutoCoerceClass(z)))

                # Dates
                z = seq.Date(as.Date("2012-03-31"), by = "year", length.out = 10)
                expect_true(is.Date(AutoCoerceClass(z)))

                # Dates as text
                expect_true(is.POSIXt(AutoCoerceClass(as.character(z))))

                # Time
                expect_true(is.timepoint(AutoCoerceClass(z + lubridate::minutes(23))))

                # Time as character
                expect_true(is.timepoint(AutoCoerceClass(as.character(z + lubridate::minutes(23)))))

                # Logical
                z <- c("TRUE", "true", "false", 'FALSE',"T", "f", "NA", "na")
                con <- AutoCoerceClass(z)
                expect_true(is.logical(con))
                expect_equal(con, c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, NA, NA))

                # numeric
                z <- c(1, 2, NA)
                expect_true(is.numeric(AutoCoerceClass(z)))

                # numeric as text
                expect_true(is.numeric(AutoCoerceClass(as.character(z))))

                # numeric as text
                expect_true(is.numeric(AutoCoerceClass(as.factor(z))))

                # factor
                z <- LETTERS[1:2]
                expect_true(is.factor(AutoCoerceClass(z)))
                expect_true(is.factor(AutoCoerceClass(as.factor(z))))
                expect_true(is.character(AutoCoerceClass(as.factor(z), stringsAsFactors = FALSE)))
                expect_true(is.factor(AutoCoerceClass(as.factor(z), stringsAsFactors = TRUE)))
                expect_true(is.character(AutoCoerceClass(as.factor(z), stringsAsFactors = TRUE, max.value.labels = 1)))

                z <- c("cat", 1, 2, NA)
                expect_true(is.factor(AutoCoerceClass(z)))
                expect_true(is.factor(AutoCoerceClass(as.factor(z))))
                expect_true(is.character(AutoCoerceClass(as.factor(z), stringsAsFactors = FALSE)))
                expect_true(is.factor(AutoCoerceClass(as.factor(z), stringsAsFactors = TRUE)))
                expect_true(is.character(AutoCoerceClass(as.factor(z), stringsAsFactors = TRUE, max.value.labels = 1)))

                z <- LETTERS[1:22]
                expect_true(is.character(AutoCoerceClass(z)))
                expect_true(is.character(AutoCoerceClass(as.factor(z))))
                expect_true(is.character(AutoCoerceClass(as.factor(z), stringsAsFactors = FALSE)))
                expect_true(is.character(AutoCoerceClass(as.factor(z), stringsAsFactors = TRUE)))
                expect_true(is.factor(AutoCoerceClass(as.factor(z), stringsAsFactors = TRUE, max.value.labels = 100)))
          })







