context("Utilities")


x <- matrix(c(0.3004, 0.6864, 0.4975, 0.2908, 0.2781, 0.2642, 0.1916, 0.284,  0.3514, 0.2534, 0.2089,
                           c(  0.0198, 0.4604, 0.2151, 0.5235, 0.1151, 0.12,   0.5457, 0.3041, 0.06312,    0.384,  0.06064),
                           c(  0.01114,    0.4111, 0.1904, 0.4494, 0.06931,    0.1112, 0.4716, 0.2859, 0.0495, 0.3296, 0.03837),
                           c(  0.01114,    0.2373, 0.089,  0.2707, 0.05322,    0.06436,    0.2756, 0.1656, 0.02967,    0.1916, 0.02228),
                           c(  0.0198, 0.177,  0.07054,    0.0297, 0.0396, 0.02719,    0.0136, 0.02847,    0.0198, 0.02847,    0.02472),
                           c(  0.4543, 0.1275, 0.07673,    0.02847,    0.07293,    0.1077, 0.01609,    0.05198,    0.321,  0.01856,    0.0297),
                           c(  0.06807,    0.1089, 0.06064,    0.0198, 0.1174, 0.04084,    0.01609,    0.01733,    0.03465,    0.01361,    0.03589),
                           c(  0.08168,    0.224,  0.1015, 0.04579,    0.04815,    0.04084,    0.03094,    0.05562,    0.05322,    0.04084,    0.02847)),nrow=8,byrow=TRUE)
x.with.labels <- x
dimnames(x.with.labels) <- list(Brand=c('Coke','V',"Red\nBull","Lift\nPlus",'Diet.Coke','Fanta','Lift','Pepsi'),
                                       Attribute=c('Kids', 'Teens',    "Enjoy life",   'Picks you up', 'Refreshes',    'Cheers you up',    'Energy',   'Up-to-date',   'Fun',  'When tired',   'Relax'))

test_that("GetTidyTwoDimensionalArray",
          {
    expect_error(GetTidyTwoDimensionalArray(x.with.labels, "NET", "NET"),NA)
    expect_error(GetTidyTwoDimensionalArray(x), NA)
    # 3D array with no names
    z <- array(NA, c(8,11,2))
    z[,,1] <- x
    expect_error(GetTidyTwoDimensionalArray(z))
    dimnames(z) <- list(dimnames(x.with.labels)[[1]], dimnames(x.with.labels)[[2]], 1:2)
    expect_error(suppressWarnings(GetTidyTwoDimensionalArray(z)), NA)
    # SUM and NET
    dimnames(x.with.labels) <- list(Brand=c('SUM','NET',"Red\nBull","Lift\nPlus",'Diet.Coke','Fanta','Lift','Pepsi'),
                                    Attribute=c('SUM', 'NET',    "Enjoy life",   'Picks you up', 'Refreshes',    'Cheers you up',    'Energy',   'Up-to-date',   'Fun',  'When tired',   'Relax'))
    GetTidyTwoDimensionalArray(x.with.labels)
})


test_that("GetTidyTwoDimensionalArray",
          {
              expect_error(GetTidyTwoDimensionalArray(x.with.labels, "NET", "NET"),NA)
              expect_error(GetTidyTwoDimensionalArray(x), NA)
              # 3D array with no names
              z <- array(NA, c(8,11,2))
              z[,,1] <- x
              expect_error(GetTidyTwoDimensionalArray(z))
              dimnames(z) <- list(dimnames(x.with.labels)[[1]], dimnames(x.with.labels)[[2]], 1:2)
              expect_error(suppressWarnings(GetTidyTwoDimensionalArray(z)), NA)
          })

test_that("DataFormula",
          {
              # R studio code editor complains about an unexpected token in the formula even though the syntax is valid
              expect_equal(DataFormula(`Profit Estimation.sav`$Variables$q1 ~ `Profit Estimation.sav`$Variables$q11),
                           `\`Profit Estimation.sav\`$Variables$q1` ~ `\`Profit Estimation.sav\`$Variables$q11`)
          })


test_that("GetTidyTwoDimensionalArray",
          {
              x <- array(1:4, dim = c(2,2))
              rownames(x) <- LETTERS[1:2]
              expect_equal(GetTidyTwoDimensionalArray(x, row.names.to.remove = "A, C"), x[2, , drop = FALSE])

          })

test_that("GetData DS-1505",
          {
              `a$b` <- runif(100)
              c <- runif(100)
              expect_equal(colnames(GetData(`a$b` ~ c, NULL, NULL)), c("`a$b`", "c"))

              df <- data.frame("a$b" = 1:3, c = 4:6, check.names = FALSE)
              expect_equal(colnames(GetData(c ~ `a$b`, df, NULL)), c("c", "a$b"))
          })

test_that("GetData missing variable",
          {
              x <- data.frame(a = seq(3), b = seq(3))
              expect_error(GetData(c ~ ., data = x), "Variable.")

          })


test_that("DS-2986: Dummy variables compatible with DataFormula",
{
    expect_equal(DataFormula(`Profit Estimation.sav`$Variables$q1 ~ `Profit Estimation.sav`$Variables$q11 +
                                 `Profit Estimation.sav`$Variables$q1.dummy.var_GQ9KqD7YOf +
                                 `Profit Estimation.sav`$Variables$q11.dummy.var_GQ9KqD7YOf),
                 `\`Profit Estimation.sav\`$Variables$q1` ~ `\`Profit Estimation.sav\`$Variables$q11` +
                     `\`Profit Estimation.sav\`$Variables$q1.dummy.var_GQ9KqD7YOf` +
                     `\`Profit Estimation.sav\`$Variables$q11.dummy.var_GQ9KqD7YOf`)
})

test_that("CleanBackticks",
{
    expect_equal(CleanBackticks("ProfitEstimation.sav$Variables$q1"),
                 "ProfitEstimation.sav$Variables$q1")
    expect_equal(CleanBackticks("`\\`Profit Estimation.sav\\`$Variables$q1`"),
                 "`Profit Estimation.sav`$Variables$q1")

    ## factor variables (levels appear as suffix in names)
    expect_equal(CleanBackticks("`ProfitEstimation.sav$Variables$Q8`100"),
                 "ProfitEstimation.sav$Variables$Q8100")
    expect_equal(CleanBackticks("`\\`Profit Estimation.sav\\`$Variables$Q8`100"),
                 "`Profit Estimation.sav`$Variables$Q8100")
})

test_that("CleanBackticks: factors with non-integer levels; DS-2884",
{

    ## factor levels might not be integers
    labels <- c("`regsNDStatewide.Final.0415.sav$Variables$Q7`0.25",
                "`regsNDStatewide.Final.0415.sav$Variables$Q7`0.5",
                "`regsNDStatewide.Final.0415.sav$Variables$Q7`0.75",
                "`regsNDStatewide.Final.0415.sav$Variables$Q7`1",
                "`regsNDStatewide.Final.0415.sav$Variables$Q8`0.25",
                "`regsNDStatewide.Final.0415.sav$Variables$Q8`0.5",
                "`regsNDStatewide.Final.0415.sav$Variables$Q8`0.75",
                "`regsNDStatewide.Final.0415.sav$Variables$Q8`1",
                "0|0.14", "0.14|0.29", "0.29|0.5", "0.5|0.71",
                "0.71|0.86", "0.86|1")
    out <- CleanBackticks(labels)
    expect_true(!any(grepl("`", out)))

    labels <- c("`\\`regsNDStatewide Final 0415.sav\\`$Variables$Q7`0.25",
                "`\\`regsNDStatewide Final 0415.sav\\`$Variables$Q7`0.5",
                "`\\`regsNDStatewide Final 0415.sav\\`$Variables$Q7`0.75",
                "`\\`regsNDStatewide Final 0415.sav\\`$Variables$Q7`1",
                "`\\`regsNDStatewide Final 0415.sav\\`$Variables$Q8`0.25",
                "`\\`regsNDStatewide Final 0415.sav\\`$Variables$Q8`0.5",
                "`\\`regsNDStatewide Final 0415.sav\\`$Variables$Q8`0.75",
                "`\\`regsNDStatewide Final 0415.sav\\`$Variables$Q8`1")
    out <- CleanBackticks(labels)
    expect_equal(out[1], "`regsNDStatewide Final 0415.sav`$Variables$Q70.25")
    expect_true(all(grepl("^`", out)))
    expect_true(all(grepl(".sav`$Variables", out, fixed = TRUE)))
})

test_that("DS-2988: Precise replacement when dataset references are used", {
    expect_equal(DataFormula(dat$Variables$xy ~ dat$Variables$x),
                 `dat$Variables$xy` ~ `dat$Variables$x`)
    expect_equal(DataFormula(`some dat`$Variables$xy ~ `some other dat`$Variables$x),
                 `\`some dat\`$Variables$xy` ~ `\`some other dat\`$Variables$x`)
})
