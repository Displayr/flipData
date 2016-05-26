# The data file that is created here has been first imported into Q, then modified in
# Q ('Data Preparation1.Q') and then saved out as an SPSS file, which is turned into an R data file here.


temp <- foreign::read.spss(system.file("extdata", "pcaPhoneTestData.sav", package = "flipData"),
                           to.data.frame = TRUE, use.value.labels = FALSE)
pcaPhoneTestData <- list(data.set = temp[,match("q2320_1", names(temp)):match("q2320_25", names(temp))],
                  data.set.original =  temp[,match("q23a", names(temp)):match("q23y", names(temp))],
                  weight = temp$wBTNOH,
                  calibrated.weight = temp$nEJYFW)
devtools::use_data(pcaPhoneTestData, internal = FALSE, overwrite = TRUE)
