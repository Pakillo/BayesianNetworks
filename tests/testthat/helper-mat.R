# Generate interaction matrices to use in tests

# valid matrices
mat_num <- matrix(c(0, 21, 14, 21, 0, 3, 27, 13,
                    0, 23, 31, 14, 29, 4, 0, 0,
                    28, 26, 1, 1, 1, 2, 0, 0,
                    1, 16, 10, 0, 1, 0, 0, 0),
                  nrow = 4, ncol = 8, byrow = TRUE,
                  dimnames = list(paste0("P", 1:4), paste0("A", 1:8)))
mat_int <- `storage.mode<-`(mat_num, "integer")

# warning matrix
mat_bin <- ifelse(mat_good > 0, 1L, 0L)  # binary

# error matrices
mat_dec <- mat_good + c(0, 0.5)  # decimals
mat_neg <- mat_good * c(1, -1)  # negative numbers

