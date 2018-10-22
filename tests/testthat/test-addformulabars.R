test("AddFormulaBars factors and continuous",
{
    dat <- data.frame(c1 = 1:3, c2 = rnorm(3), f1 = factor(1:3), f2 = ordered(1:3))
    frml <- ~c1+c2+f1+f2
    AddFormulaBars(frml, dat)
    expect_equal(AddFormulaBars(frml, dat), as.formula("~c1+c2+(1|f1)+(1|f2)"))
})

test("AddFormulaBars continuous only",
{
    dat <- data.frame(c1 = 1:3, c2 = rnorm(3), f1 = factor(1:3), f2 = ordered(1:3))
    frml <- ~c1
    expect_equal(AddFormulaBars(frml, dat), frml)

    frml <- ~c1+c2
    expect_equal(AddFormulaBars(frml, dat), frml)
})

test("AddFormulaBars factors and continuous",
{
    dat <- data.frame(c1 = 1:3, c2 = rnorm(3), f1 = factor(1:3), f2 = ordered(1:3))
    frml <- ~f1
    expect_equal(AddFormulaBars(frml, dat), as.formula("~(1|f1)"))

    frml <- ~f1+f2
    expect_equal(AddFormulaBars(frml, dat), as.formula("~(1|f1)+(1|f2)"))
})
