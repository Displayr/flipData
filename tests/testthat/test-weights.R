library(testthat)
context("Weights")


test_that("Effective Sample Size",
{
    expect_equal(EffectiveSampleSize(rep(1,100)), 100)
    expect_equal(EffectiveSampleSize(c(10, rep(1,9))), 3.311927, tolerance = 0.00001)
    expect_error(EffectiveSampleSize(c(Inf, rep(1,9))))
    expect_error(EffectiveSampleSize(c(-1, rep(1,9))))
    expect_error(EffectiveSampleSize(c(NA, rep(1,9))))
})


#
# ## one-stage cluster sample
#
# dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
#
# svymean(~api00, dclus1, deff=TRUE)
# svymean(~factor(stype),dclus1)
# svymean(~interaction(stype, comp.imp), dclus1)
# svyquantile(~api00, dclus1, c(.25,.5,.75))
# svytotal(~enroll, dclus1, deff=TRUE)
# svyratio(~api.stu, ~enroll, dclus1)
#
# v<-svyvar(~api00+api99, dclus1)
# v
# print(v, cov=TRUE)
# as.matrix(v)
#
# # replicate weights - jackknife (this is slower)
# dstrat<-svydesign(id=~1,strata=~stype, weights=~pw,
#                   data=apistrat, fpc=~fpc)
# jkstrat<-as.svrepdesign(dstrat)
#
# svymean(~api00, jkstrat)
# svymean(~factor(stype),jkstrat)
# svyvar(~api00+api99,jkstrat)
#
# svyquantile(~api00, jkstrat, c(.25,.5,.75))
# svytotal(~enroll, jkstrat)
# svyratio(~api.stu, ~enroll, jkstrat)
#
# # coefficients of variation
# cv(svytotal(~enroll,dstrat))
# cv(svyratio(~api.stu, ~enroll, jkstrat))
#
# # extracting information from the results
# coef(svytotal(~enroll,dstrat))
# vcov(svymean(~api00+api99,jkstrat))
# SE(svymean(~enroll, dstrat))
# confint(svymean(~api00+api00, dclus1))
#
# # Design effect
# svymean(~api00, dstrat, deff=TRUE)
# svymean(~api00, dstrat, deff="replace")
# svymean(~api00, jkstrat, deff=TRUE)
# svymean(~api00, jkstrat, deff="replace")
# (a<-svytotal(~enroll, dclus1, deff=TRUE))
# deff(a)
