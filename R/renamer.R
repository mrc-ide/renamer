recode_character_match <- function(x, table) {
  x_clean <- basic_clean(x)
  known <- table$from
  known_clean <- basic_clean(known)
  i <- match(x_clean, known_clean)
  is_unmatched <- nzchar(x_clean) & is.na(i) & !is.na(x)
  list(
    is_unmatched = is_unmatched,
    input = x,
    input_clean = x_clean,
    output = table$to[i],
    known = known,
    known_clean = known_clean,
    all_matched = !any(is_unmatched))
}


recode_character_suggest <- function(x, table, threshhold = 0.3, n = 2L) {
  res <- recode_character_match(x, table)

  if (!res$all_matched) {
    i <- which(res$is_unmatched)[order(res$input_clean[res$is_unmatched])]
    missed <- res$input[i]
    missed_clean <- res$input_clean[i]
    target <- res$known_clean
    to <- res$known

    d <- stringdist::stringdistmatrix(missed_clean, target, method = "jw")

    ## This does not capture *how* we get to a match but that's
    ## generally pretty hard to convey and probably not that
    ## interesting.
    best_to <- function(r) {
      ret <- unique(to[rank(r, ties.method = "first")[r < threshhold]])
      if (length(ret) < n) {
        ret <- c(ret, rep_len(NA_character_, n - length(ret)))
      } else {
        ret <- ret[seq_len(n)]
      }
      ret
    }

    suggestions <- t(apply(d, 1, best_to))
    colnames(suggestions) <-
      sprintf("suggestion_%d", seq_len(ncol(suggestions)))

    j <- order(suggestions[, 1])
    res$suggestions <- data_frame(
      from = missed[j],
      clean = missed_clean[j],
      suggestions[j, ])
  }
  res
}


## This will be made configurable I think
basic_clean <- function(x) {
  ## First sanitise whitespace:
  x <- gsub("\\s+", " ", trimws(x))
  ## transliterate accents
  Encoding(x) <- "UTF-8"
  x <- stringi::stri_trans_general(x, "latin-ASCII")
  ## capitalisation:
  x <- tolower(x)
  ## remove all other characters:
  x <- gsub("[^a-z ]", "", x)

  x
}
