phone <- foreign::read.spss(system.file("extdata", "phone.sav", package = "flipData"))
devtools::use_data(phone, internal = FALSE, overwrite = TRUE)

