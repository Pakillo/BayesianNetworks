## code to prepare `DATASET` dataset goes here

dt <- read.table("https://raw.githubusercontent.com/jg-you/plant-pollinator-inference/master/example_matrix.txt")
web <- apply(dt, c(1,2), as.integer)
rownames(web) <- paste0("P", 1:nrow(web))
colnames(web) <- paste0("A", 1:ncol(web))

usethis::use_data(web, overwrite = TRUE)
