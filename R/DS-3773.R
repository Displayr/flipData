# d <- haven::read_sav("wincross exported file sr wave 1.sav")
# attr(d[[1]], "labels") <- NULL
# haven::write_sav(d, "wincross exported file sr wave mod.sav")
#
# d2 <- haven::read_sav("fs22001 sr brand wave 2.sav")
# attr(d2[["Q5r97"]], "labels") <- NULL
# haven::write_sav(d2, "fs22001 sr brand wave mod.sav")
#
# a <- MergeDataSetsByCase(c("wincross exported file sr wave mod.sav", "fs22001 sr brand wave mod.sav"),
#                          auto.select.what.to.match.by = FALSE, match.by.variable.names = TRUE,
#                          match.by.variable.labels = FALSE, match.by.value.labels = FALSE,
#                          ignore.non.alphanumeric = FALSE)

# Issues:
# RECORD/record variables not merging (one is numeric, one categorical)
# Q5R97 merged into Q5_r97 instead of Q5r97
# flipFormat::DataSetMergingByCaseWidget crashing when printing output when non-categorical input variables are merged and converted to categorical
# Rename merged variables if they have the same name (ignoring case)
