library(testthat)

context("Tests for the R Code Weights Dialog in Displayr")

findInstDirFile <- function(file)
{
    file.path(system.file("testdata", package = "flipData", mustWork = TRUE),
              file)
}

weighted.table = function(weight, variable) { prop.table(tapply(weight, variable, sum)) }

marriage = foreign::read.spss(findInstDirFile("Marriage.sav"), to.data.frame = TRUE)

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

    expect_error(WeightingDialog(categorical.variables = data.frame(input.age, input.gender, input.region),
                 categorical.targets=list(variable.targets.age2, variable.targets.gender, variable.targets.region)),
                                 "This should be processed via the existing Q algorithm and this code should not have been called.")

})

# Comparing all algorithms with a single categorical adjustment variable
test_that("Single Categorical adjustment variable", {

    # Raking with no bounds and no numeric
    expect_error(WeightingDialog(list(Age = input.age), variable.targets.age),
                 "This should be processed via the existing Q algorithm and this code should not have been called.")

    # Raking with trivial lower and upper values
    wgt.calibrate = Calibrate(list(Age = input.age), variable.targets.age, package = "icarus", always.calibrate = TRUE)
    wgt.dialog = WeightingDialog(list(Age = input.age), variable.targets.age, lower = 0, upper = 1000)
    expect_equivalent(cor(wgt.calibrate, wgt.dialog), 1)

    # linear
    wgt.dialog = WeightingDialog(list(Age = input.age), variable.targets.age, lower = 0, upper = 1000, calfun = "linear")
    expect_equivalent(cor(wgt.calibrate, wgt.dialog), 1)

    # logit
    wgt.dialog = WeightingDialog(list(Age = input.age), variable.targets.age, lower = 0, upper = 1000, calfun = "logit")
    expect_equivalent(cor(wgt.calibrate, wgt.dialog), 1)
})


for (calfun in c("Raking", "Linear", "Logit")){

    test_that("Numeric input", {
        numeric.variables = list(input.race.white, input.race.black, input.race.hispanic)
        wgt = WeightingDialog(numeric.variables = numeric.variables,
                            numeric.targets = variable.targets.race,
                            calfun = calfun)
        actual.weighted.means <- weighted.table(wgt, input.race)
        expect_equivalent(actual.weighted.means, variable.targets.race)
    })

    test_that("Categorical and Numeric input", {
        wgt = WeightingDialog(
            categorical.variables=input.age,
            categorical.targets=variable.targets.age,
            numeric.variables = data.frame(input.race.white, input.race.black, input.race.hispanic),
            numeric.targets=variable.targets.race, calfun = calfun
        )

        actual.weighted.means <- tapply(wgt, input.race, sum) / length(input.race)
        expect_equivalent(actual.weighted.means, variable.targets.race, tol = 0.0000001)

        actual.weighted.means <- tapply(wgt, input.age, sum) / length(input.age)
        expect_equivalent(actual.weighted.means, as.numeric(variable.targets.age[5:8]), tol = 0.0000001)
})

}


test_that("Calmar examples: http://vesselinov.com/CalmarEngDoc.pdf",
          { # Note that we compare the resulting statisics, but not the weights themselves
            # as there are an infinite number of possible weights.
              # 1 numeric
              x = c(1,1,1,2,2,4,6,7,8,11,12,10,15,20,50)
              #variable.targets.age = structure(c("18-29", "30-44", "45-64", "65+", ".4", ".3", ".2", ".1"), .Dim = c(4L, 2L))
              target.x = mean(x) * 1400 / 1200
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(numeric.variables = data.frame(x),
                                        numeric.targets = target.x, calfun = calfun)
                  expect_equal(sum(x * wgt) * 8, 1400 , tol = 0.1)
              }
              # Adding a constraint
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(numeric.variables = data.frame(x),
                                        numeric.targets = target.x, calfun = calfun,
                                        lower = 0.9)
                  expect_equal(sum(x * wgt) * 8, 1400, tol = 0.1)
              }

              # 1 numeric and 1 categorical
              y = factor(c("a","a","b","b","c","a","c","c","b","a","a","b","c","c","b"))
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(data.frame(y),
                                        structure(c("a", "b", "c", prop.table(c(38, 41, 39))), .Dim = c(3L, 2L)),                                        numeric.variables = data.frame(x),
                                        numeric.targets = target.x, calfun = calfun)
                  expect_equal(sum(x * wgt) * 8, 1400, tol = 0.1)
                  expect_equal(sum(prop.table(wgt)[y == "a"]), 38/118, tol = 0.0001)
                  expect_equal(sum(prop.table(wgt)[y == "b"]), 41/118, tol = 0.0001)
                  expect_equal(sum(prop.table(wgt)[y == "c"]), 39/118, tol = 0.0001)
              }
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(data.frame(y),
                                        structure(c("a", "b", "c", prop.table(c(38, 41, 39))), .Dim = c(3L, 2L)),                                        numeric.variables = data.frame(x),
                                        numeric.targets = target.x, calfun = calfun)
                  expect_equal(sum(x * wgt) * 8, 1400 , tol = 0.1)
                  expect_equal(sum(prop.table(wgt)[y == "a"]), 38/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(prop.table(wgt)[y == "b"]), 41/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(prop.table(wgt)[y == "c"]), 39/118, tol = 0.0001, lower = 0.9)
              }
          })

test_that("Grossing for a numeric variable (does nothing)",
          {
              x = c(1,1,1,2,2,4,6,7,8,11,12,10,15,20,50)
              target.x = mean(x) * 1400 / 1200
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(numeric.variables = data.frame(x),
                                        numeric.targets = target.x,
                                        calfun = calfun,
                                        force.to.n = FALSE)
                  expect_equal(sum(x * wgt) * 8, 1400 , tol = 0.1)
              }
          })



test_that("A numeric variable with a design weight",
          {
              x = c(1,1,1,2,2,4,6,7,8,11,12,10,15,20,50)
              set.seed(1223)
              d.weight <- runif(length(x)) * 100
              target.x = mean(x) * 1400 / 1200
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(numeric.variables = data.frame(x),
                                        numeric.targets = target.x,
                                        calfun = calfun,
                                        input.weight = d.weight)
                  expect_equal(sum(x * wgt) * 8, 1400 , tol = 0.1)
              }
          })
test_that("Grossing for a numeric variable (does nothing) with a design weight",
          {
              x = c(1,1,1,2,2,4,6,7,8,11,12,10,15,20,50)
              set.seed(1223)
              d.weight <- runif(length(x)) * 100
              target.x = mean(x) * 1400 / 1200
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(numeric.variables = data.frame(x),
                                        numeric.targets = target.x,
                                        calfun = calfun,
                                        input.weight = d.weight,
                                        force.to.n = FALSE)
                  expect_equal(sum(x * wgt) * 8, 1400 , tol = 0.1)
              }
          })

test_that("Totals for a categorical adjustment variable",
          {
              x = c(1,1,1,2,2,4,6,7,8,11,12,10,15,20,50)
              y = factor(c("a","a","b","b","c","a","c","c","b","a","a","b","c","c","b"))
              target.x = mean(x) * 1400 / 1200
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(data.frame(y),
                                        structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L)),                                        numeric.variables = data.frame(x),
                                        numeric.targets = target.x, calfun = calfun)
                  expect_equal(sum(x * wgt) * 8, 1400 , tol = 0.1)
                  expect_equal(sum(prop.table(wgt)[y == "a"]), 38/118, tol = 0.0001, lower = "0.9")
                  expect_equal(sum(prop.table(wgt)[y == "b"]), 41/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(prop.table(wgt)[y == "c"]), 39/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt), length(x))
              }
          })



test_that("Grossing + Totals for a categorical adjustment variable",
          {
              x = c(1,1,1,2,2,4,6,7,8,11,12,10,15,20,50)
              y = factor(c("a","a","b","b","c","a","c","c","b","a","a","b","c","c","b"))
              target.x = mean(x) * 1400 / 1200
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(data.frame(y),
                                        structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L)),                                        numeric.variables = data.frame(x),
                                        numeric.targets = target.x,
                                        calfun = calfun,
                                        force.to.n = FALSE)
                  expect_equal(sum(x * wgt), 1400 , tol = 0.1)
                  expect_equal(sum(wgt[y == "a"]), 38, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt[y == "b"]), 41, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt[y == "c"]), 39, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt), 118)
              }
          })


test_that("Grossing + Totals for two categorical adjustment variables",
          {
              x = c(1,1,1,2,2,4,6,7,8,11,12,10,15,20,50)
              y = factor(c("a","a","b","b","c","a","c","c","b","a","a","b","c","c","b"))
              target.x = mean(x) * 1400 / 1200
              # The same variable in twice
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(data.frame(y1 = y, y2 = y),
                                        list(structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L)),
                                             structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L))),
                                             numeric.variables = data.frame(x),
                                        numeric.targets = target.x,
                                        calfun = calfun,
                                        force.to.n = FALSE)
                  expect_equal(sum(x * wgt), 1400 , tol = 0.1)
                  expect_equal(sum(wgt[y == "a"]), 38, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt[y == "b"]), 41, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt[y == "c"]), 39, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt), 118)
              }
              # The same variable in twice - counts first, then percent
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(data.frame(y1 = y, y2 = y),
                                        list(structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L)),
                                             structure(c("a", "b", "c", prop.table(c(38, 41, 39))), .Dim = c(3L, 2L))),
                                        numeric.variables = data.frame(x),
                                        numeric.targets = target.x,
                                        calfun = calfun,
                                        force.to.n = FALSE)
                  expect_equal(sum(x * wgt), 1400 , tol = 0.1)
                  expect_equal(sum(wgt[y == "a"]), 38, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt[y == "b"]), 41, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt[y == "c"]), 39, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt), 118)
              }
              # The same variable in twice - percent first, then count
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt = WeightingDialog(data.frame(y1 = y, y2 = y),
                                        list(structure(c("a", "b", "c", prop.table(c(38, 41, 39))), .Dim = c(3L, 2L)),
                                             structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L))),
                                        numeric.variables = data.frame(x),
                                        numeric.targets = target.x,
                                        calfun = calfun,
                                        force.to.n = FALSE)
                  expect_equal(sum(x * wgt) , 11.6667 , tol = 0.1)
                  expect_equal(sum(prop.table(wgt)[y == "a"]), 38/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(prop.table(wgt)[y == "b"]), 41/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(prop.table(wgt)[y == "c"]), 39/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt), 1)
              }
          })



test_that("Totals for two categorical adjustment variables with a design weight",
          {
              x = c(1,1,1,2,2,4,6,7,8,11,12,10,15,20,50)
              y = factor(c("a","a","b","b","c","a","c","c","b","a","a","b","c","c","b"))
              set.seed(1223)
              d.weight <- runif(length(x)) * 100
              target.x = mean(x) * 1400 / 1200
              # The same variable in twice
              for (calfun in c("Raking", "Linear", "Logit"))
              {
                  wgt.no.design = WeightingDialog(data.frame(y1 = y, y2 = y),
                                          list(structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L)),
                                               structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L))),
                                          numeric.variables = data.frame(x),
                                          numeric.targets = target.x,
                                          calfun = calfun,
                                          force.to.n = TRUE)
                  wgt = WeightingDialog(data.frame(y1 = y, y2 = y),
                                          list(structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L)),
                                               structure(c("a", "b", "c", c(38, 41, 39)), .Dim = c(3L, 2L))),
                                          numeric.variables = data.frame(x),
                                          numeric.targets = target.x,
                                          calfun = calfun,
                                          force.to.n = TRUE,
                                          input.weight = d.weight)
                  # The design weight should cause the weight to be different
                  expect_true(cor(wgt,wgt.no.design ) < 1)
                  # But its results should be the same
                  expect_equal(sum(x * wgt) * 8, 1400 , tol = 0.1)
                  expect_equal(sum(prop.table(wgt)[y == "a"]), 38/118, tol = 0.0001, lower = "0.9")
                  expect_equal(sum(prop.table(wgt)[y == "b"]), 41/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(prop.table(wgt)[y == "c"]), 39/118, tol = 0.0001, lower = 0.9)
                  expect_equal(sum(wgt), length(x))
              }
          })
