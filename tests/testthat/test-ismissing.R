context("IsMissing")


for (type in 1:3)
{
    example <- list(1, "one", TRUE)[type]
    test_that(paste("AutoCoerceType", c("numeric", "character", "logical")[type]),
    {
        expect_true(IsMissing(c(example,NA))[2])
        expect_true(IsMissing(c(example,"NA"))[2])
        expect_true(IsMissing(c(example,"na"))[2])
        expect_true(IsMissing(c(example,"null"))[2])
        expect_true(IsMissing(c(example,"NULL"))[2])
        expect_true(IsMissing(c(example,"Missing"))[2])
        expect_true(IsMissing(c(example,""))[2])
        expect_true(IsMissing(c(example,"."))[2])
        expect_true(IsMissing(c(example,"      "))[2])
    })
}
