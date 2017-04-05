# helper function to convert a hexadecimals string to a raw vector.
# note: This is a reduced version of the code provided at https://github.com/ianmcook/wkb/blob/master/R/hex2raw.R.
hex2raw <- function(hex) {
    hex <- strsplit(hex, character(0))[[1]]
    hex <- paste(hex[c(TRUE, FALSE)], hex[c(FALSE, TRUE)], sep = "")
    as.raw(as.hexmode(hex))
}