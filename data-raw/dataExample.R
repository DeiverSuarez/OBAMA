## code to prepare `dataExample` dataset goes here
dataExample <- read.csv("C:/Users/didier.murilloflorez/Documents/NEWAGM7621_1.CSV", header = TRUE)
usethis::use_data(dataExample, overwrite = TRUE, internal = FALSE)

