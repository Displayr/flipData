library(testthat)

context("Calibrate")

weighted.table = function(weight, variable) { prop.table(tapply(weight, variable, sum)) }

marriage = foreign::read.spss("https://docs.displayr.com/images/8/89/Marriage.sav", to.data.frame = TRUE)

input.weight = marriage$weight

input.age = marriage$age.cat
input.gender = marriage$female
input.race = marriage$race.wbh
input.race.white = as.integer(input.race == "White")
input.race.black = as.integer(input.race == "Black")
input.race.hispanic = as.integer(input.race == "Hispanic")
marriage$region.cat = as.character(marriage$region.cat)
marriage$region.cat[marriage$region.cat %in% c("DC", "Northeast")] = "Northeast + DC"
marriage$region.cat = factor(marriage$region.cat)
input.region = marriage$region.cat

variable.targets.age = structure(c("18-29", "30-44", "45-64", "65+", ".4", ".3", ".2", ".1"), .Dim = c(4L, 2L))
variable.targets.race = structure(c(0.8, 0.1, 0.1))
variable.targets.age2 = structure(c("18-29", "30-44", "45-64", "65+", ".2", ".3",".31", ".19"), .Dim = c(4L, 2L))
variable.targets.gender = structure(c("Male", "Female", "0.45", "0.55"), .Dim=c(2L, 2L))
variable.targets.region = structure(c("Midwest", "Northeast + DC", "South", "West", ".27", "0.26", "0.24", "0.23"), .Dim=c(4L, 2L))

test_that("Multiple Categorical inputs", {

    wgt = Calibrate(categorical.variables = data.frame(input.age, input.gender, input.region),
                        categorical.targets=list(variable.targets.age2, variable.targets.gender, variable.targets.region))

    expect_equivalent(weighted.table(wgt, input.region), as.numeric(variable.targets.region[5:8]))

    expect_equivalent(weighted.table(wgt, input.age), as.numeric(variable.targets.age2[5:8]), tol = 0.0000001)

    expect_equivalent(weighted.table(wgt, input.gender), as.numeric(variable.targets.gender[3:4]))

})

# Comparing all algorithms with a single categorical adjustment varibale
test_that("Single Categorical adjustment varibale", {

    # Default: Raking
    wgt = Calibrate(list(Age = input.age), variable.targets.age, package = "survey", always.calibrate = FALSE)
    expect_equivalent(weighted.table(wgt, input.age), as.numeric(variable.targets.age[5:8]), tol = 0.0000001)

    # Default: icarus
    wgt = Calibrate(list(Age = input.age), variable.targets.age, package = "icarus", always.calibrate = TRUE)
    expect_equivalent(weighted.table(wgt, input.age), as.numeric(variable.targets.age[5:8]), tol = 0.0000001)

    # Default: CVXR
    wgt = Calibrate(list(Age = input.age), variable.targets.age, package = "CVXR", always.calibrate = TRUE)
    expect_equivalent(weighted.table(wgt, input.age), as.numeric(variable.targets.age[5:8]), tol = 0.0000001)

    # Default: survey calibrate
    wgt = Calibrate(list(Age = input.age), variable.targets.age, package = "survey", always.calibrate = TRUE)
    expect_equivalent(weighted.table(wgt, input.age), as.numeric(variable.targets.age[5:8]), tol = 0.0000001)
})


for (package in c("survey", "icarus"))
    test_that("Numeric input", {
        numeric.variables = list(input.race.white, input.race.black, input.race.hispanic)
        wgt = Calibrate(numeric.variables = numeric.variables,
                            numeric.targets = variable.targets.race,
                            package = package)
        actual.weighted.means <- weighted.table(wgt, input.race)
        expect_equivalent(actual.weighted.means, variable.targets.race)
    })

test_that("Categorical and Numeric input", {
    wgt = Calibrate(
        categorical.variables=input.age,
        categorical.targets=variable.targets.age,
        numeric.variables = data.frame(input.race.white, input.race.black, input.race.hispanic),
        numeric.targets=variable.targets.race
    )

    actual.weighted.means <- tapply(wgt, input.race, sum) / length(input.race)
    expect_equivalent(actual.weighted.means, variable.targets.race, tol = 0.0000001)

    actual.weighted.means <- tapply(wgt, input.age, sum) / length(input.age)
    expect_equivalent(actual.weighted.means, as.numeric(variable.targets.age[5:8]), tol = 0.0000001)


})

test_that("Min/Max weights", {
    unbounded <- Calibrate(numeric.variables = list(input.race.white, input.race.black, input.race.hispanic),
                           numeric.targets = variable.targets.race)
    actual <- Calibrate(numeric.variables = list(input.race.white, input.race.black, input.race.hispanic),
                        numeric.targets=variable.targets.race,
                        lower=0.8,
                        upper=1.4
                        )
    expect_gte(min(actual), min(unbounded))
    expect_lte(max(actual), max(unbounded))
})

test_that("Invalid inputs", {
    expect_error(Calibrate(), "Nothing to do")
    expect_error(Calibrate(input.age), "The number of")
    expect_error(Calibrate(numeric.variables = list(input.race.white, input.race.black, input.race.hispanic)), "The number of ")
    expect_error(Calibrate(input.age, variable.targets.age[-2,]), "No targets.*30-44")

    variable.targets.age.errored = variable.targets.age
    variable.targets.age.errored[6] = .6
    expect_error(Calibrate(input.age, variable.targets.age.errored), "add up to exactly 1")

    variable.targets.age.errored[6] = NA
    expect_error(Calibrate(input.age, variable.targets.age.errored), "Invalid target values")

    variable.targets.age.errored = structure(c("18-29", "30-44", "45-64", "65+", "should be dead already", ".20", ".20",
                                       ".20", ".20", ".20"), .Dim = c(5L, 2L))
    expect_error(Calibrate(input.age, variable.targets.age.errored), "does not appear in variable")

    ## DS-2846
    variable.targets.age.ws  <-  variable.targets.age
    variable.targets.age.ws[1, 2] <- "   .4"
    variable.targets.age.ws[2, 2] <- "   .3    "
    expect_error(Calibrate(input.age, variable.targets.age.ws), NA)

    variable.targets.age.errored  <-  variable.targets.age
    variable.targets.age.errored[1, 2] <- "x.4"
    expect_error(Calibrate(input.age, variable.targets.age.errored), "x: 18-29 - x.4")
})

# test_that("Print function (depends on file: helper-globalVars.R)", {
#     out = capture.output(print(Calibrate(input.age, variable.targets.age)))
#     expect_equal(out[1], "Effective sample size: 1,775 (91%)")
#     expect_equal(out[3], "To save the variable, click Automate > Browse Online Library > Weighting > Save Variable")
# })
#

test_that("Subset",
{
    no.subset.weight = Calibrate(input.age, variable.targets.age, subset = TRUE)

    # Explicitly passing in subset as NULL
    expect_equal(Calibrate(input.age, variable.targets.age, subset = NULL), no.subset.weight)

    # Passing in subset as TRUE (Displayr's value when no filter is provided)
    expect_equal(Calibrate(input.age, variable.targets.age, subset = rep(TRUE, length(input.age))), no.subset.weight)

    # Passing in subset with random values
    set.seed(1223)
    sbst = runif(length(input.age)) > .5
    wgt = Calibrate(input.age, variable.targets.age, subset = sbst)
    # Checking that weights are provided for the subsetted values
    expect_true(all(sbst == !is.na(wgt)))

    # Checking that the weighted proportion matches the target
    expect_equivalent(sum(wgt[input.age == '18-29'], na.rm = TRUE) / sum(wgt, na.rm = TRUE), as.numeric(variable.targets.age[1, 2]), tol = 0.0000001)
})

test_that("input.weight",
          {
              # Checking that when input.weight is supplied, the resulting weight is more correlated with it than otherwise
              wgt = Calibrate(input.age, variable.targets.age)
              wgt.with.input.weight = Calibrate(input.age, variable.targets.age, input.weight = input.weight)
              expect_true(cor(input.weight, wgt) < cor(input.weight, wgt.with.input.weight))

              # Checking that input.weight works with subsetting
              set.seed(1224)
              sbst = runif(length(input.age)) > .5
              wgt = Calibrate(input.age, variable.targets.age, subset = sbst)
              wgt.with.input.weight = Calibrate(input.age, variable.targets.age, input.weight = input.weight, subset = sbst)
              expect_true(cor(input.weight, wgt, use = "pairwise.complete.obs") < cor(input.weight, wgt.with.input.weight, use = "pairwise.complete.obs"))
              expect_equivalent(sum(wgt.with.input.weight[input.age == '18-29'], na.rm = TRUE) / sum(wgt.with.input.weight, na.rm = TRUE),
                                as.numeric(variable.targets.age[1, 2]))
          })

test_that("trimming",
          {
              # Checking that un-trimmed average is o
              wgt = Calibrate(input.age, variable.targets.age)
              rng = diff(range(wgt))
              expect_equal(mean(wgt), 1)

              # Checking that average remaines 0
              wgt = Calibrate(input.age, variable.targets.age, lower = 0.7, upper = 1.5)
              expect_equal(mean(wgt), 1)

              # Checking that trimming reduces the range
              expect_true(diff(range(wgt)) < rng)


              # Checking that trim.iterations does something
              wgt = Calibrate(input.age, variable.targets.age, lower = 0.7, upper = 1.5, trim.iterations = 0)
              rng = diff(range(wgt))
              expect_true(diff(range(wgt)) == rng)

              # Checking that trimming works with subset
              set.seed(1225)
              sbst = runif(length(input.age)) > .5

              # Checking that un-trimmed average is o
              wgt = Calibrate(input.age, variable.targets.age, subset = sbst)
              rng = diff(range(wgt, na.rm = TRUE))
              expect_equal(mean(wgt, na.rm = TRUE), 1)

              # Checking that average remaines 0
              wgt = Calibrate(input.age, variable.targets.age, lower = 0.7, upper = 1.5, subset = sbst)
              expect_equal(mean(wgt, na.rm = TRUE), 1)

              # Checking that trimming reduces the range
              expect_true(diff(range(wgt, na.rm = TRUE)) < rng)

              # Checking that trim.iterations does something
              wgt = Calibrate(input.age, variable.targets.age, lower = 0.7, upper = 1.5, trim.iterations = 0, subset = sbst)
              rng = diff(range(wgt, na.rm = TRUE))
              expect_true(diff(range(wgt, na.rm = TRUE)) == rng)
})



test_that("Ordering of categories in a categorical adjustment variable makes no difference",
          {
              adj.variable = list(Gender = marriage$female)
              wgt = Calibrate(adj.variable, list(cbind(c("Male", "Female"), c( .75, .25))))
              expect_equivalent(weighted.table(wgt, adj.variable)["Male"],c("Male" = .75))

              wgt = Calibrate(adj.variable, list(cbind(c("Female", "Male"), c( .75, .25))))
              expect_equivalent(weighted.table(wgt, adj.variable)["Female"], c("Female" = .75))

              wgt = Calibrate(list(Region = marriage$region_cat), list(cbind(c("west", "northeast", "south", "midwest", "dc"), c( .25, .25, .25, .24, 0.01))))
              wgt1 = Calibrate(list(marriage$region_cat), list(cbind(c("dc", "midwest", "northeast", "south", "west"), c(0.01, .24, .25, .25, .25))))
              expect_equal(wgt, wgt1)
          })


test_that("Subset that causes a factor level to disappear (i.e., dc)",
          {
              adj.variable = list(Region = marriage$region_cat)
              filt = marriage$female == "Male"
              wgt = Calibrate(adj.variable, list(cbind(c("midwest", "northeast", "south", "west"), c(.23, .24, .26, .27))),
                        subset = filt)

              totals = tapply(wgt[filt], list(factor(adj.variable[[1]][filt])), sum)
              expect_equivalent(prop.table(totals)["south"],c("south" = .26))

          })

test_that("Problem for which calibration fails (due to poor algorithms)",
          {
              adjustment.variable = list(Region = marriage$region_cat)
              category.names = levels(adjustment.variable[[1]])
              target.proportions = c(.02, 0.24, .24, .25, .25)
              # Raking - should not have an error
              expect_error(Calibrate(adjustment.variable, list(cbind(category.names, target.proportions)), always.calibrate = FALSE), NA)

              # Using calibration
              expect_error(suppressWarnings(capture.output(Calibrate(adjustment.variable, list(cbind(category.names, target.proportions)),
                                          always.calibrate = TRUE,
                                          package = "survey"))))

              # Using calibration
              expect_error(capture.output(Calibrate(adjustment.variable, list(cbind(category.names, target.proportions)),
                                          always.calibrate = TRUE,
                                          package = "icarus")), NA)

              # Using calibration
              expect_error(capture.output(Calibrate(adjustment.variable, list(cbind(category.names, target.proportions)),
                                          always.calibrate = TRUE,
                                          package = "CVXR")), NA)
          })

test_that("RS-2527 Targets of 0: automatically switching to calbration",
          {
              adjustment.variable = list(Region = marriage$region_cat)
              category.names = levels(adjustment.variable[[1]])
              target.proportions = c(.00, 0.26, .24, .25, .25)
              # Raking - should have an error
              expect_error(Calibrate(adjustment.variable, list(cbind(category.names, target.proportions)), always.calibrate = FALSE), "One of your targets")

              # Raking with a filter- should not have an error
              expect_error(Calibrate(adjustment.variable,
                                     list(cbind(category.names, target.proportions)[-1, ]),
                                     always.calibrate = FALSE,
                                     subset = marriage$region_cat != "dc"), NA)

              # Calibrate - should not have an error
              expect_error(Calibrate(adjustment.variable, list(cbind(category.names, target.proportions)), always.calibrate = TRUE), NA)


          })

test_that("Output contains the right class for extension buttons", {
    # NOTE: if the tests below fails due to class names changing, ALL
    #       extension buttons in the wiki that refer to this class name should
    #       be updated with the new class name.

    wgt <- Calibrate(categorical.variables = data.frame(input.age, input.gender, input.region),
                     categorical.targets=list(variable.targets.age2, variable.targets.gender, variable.targets.region))

    expect_true(inherits(wgt, "Calibrate"))
})

test_that("DS-3458: Catch CVXR solver errors from bad input data",
{
    numeric.vars <- list(Coke = c(10, 5, 0),
                         Pepsi = c(0, 5, 2))
    targets <- c(.5, .5)
    expect_error(Calibrate(numeric.variables = numeric.vars,
                           numeric.targets = targets),
                 "check that the supplied targets are appropriate for your data.")
})

test_that("DS-3646: Always drop empty levels when checking validity of targets",
{
    categorical.vars <- list(Gender = factor(c("Male", "Female", "Male", "Female")),
                             Age = factor(c("Old", "Old", "Young", "Young"), levels = c("Old", "Young", "Middle-aged")))
    targets <- list(Gender = rbind(c("Male", 0.5), c("Female", 0.5)),
                    Age = rbind(c("Old", 0.25), c("Young", 0.25), c("Middle-aged",0.5)))
    expect_error(Calibrate(categorical.vars, targets),
                 "does not appear")

})

test_that("DS-3682: Normalize rake weight before trimming", {
    upper = 2
    lower = 0.3
    x = Calibrate(list(Age = input.age, Gender = input.gender), 
                  list(Age = variable.targets.age, Gender = variable.targets.gender), 
                  upper = upper, 
                  lower = lower)
    expect_equal(round(min(x), 7), 0.4089635)
    expect_equal(round(max(x), 6), 2.148377)
})
