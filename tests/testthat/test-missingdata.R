context("Missing Data")
data(bank, package = "flipExampleData")

dat <- data.frame(a = rep((1:10)/10,2),
    b = rep(1:10,2),
    c = factor(rep(c(rep("A",5), rep("B",5)),2)),
    d = ordered(rep(c(rep("A",5), rep("B",5)),2)), e = rep("dog",20), stringsAsFactors = FALSE)
for (i in 1:5)
    dat[i, i] <- NA

test_that("Missing options",
{
    expect_error(EstimationData(Overall ~ Branch, bank, missing = "Error if missing data")$estimation.data)
    expect_equal(341, suppressWarnings(nrow(EstimationData(Overall ~ Branch, bank, missing = "Exclude cases with missing data")$estimation.data)))
    expect_equal(823, nrow(EstimationData(Overall ~ Branch, bank, missing = "Use partial data")$estimation.data))
    expect_equal(823, nrow(EstimationData(Overall ~ Branch, bank, missing = "Use partial data (pairwise correlations)")$estimation.data))
})

