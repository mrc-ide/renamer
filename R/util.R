read_csv <- function(...) {
  read.csv(..., stringsAsFactors = FALSE)
}


write_csv <- function(...) {
  write.csv(..., row.names = FALSE)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}
