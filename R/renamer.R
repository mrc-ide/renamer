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
    d_best <- apply(d, 1, min)

    ## This does not capture *how* we get to a match but that's
    ## generally pretty hard to convey and probably not that
    ## interesting.
    best_to <- function(r) {
      ret <- unique(to[rank(r, ties.method = "first")[r <= threshhold]])
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

    j <- order(suggestions[, 1], d_best)
    res$suggestions <- data_frame(
      from = missed[j],
      suggestions[j, ],
      distance_best = round(d_best[j], 2))
  }
  res
}


recode_character_iterate <- function(x, filename_table, filename_new = NULL,
                                     ..., overwrite = FALSE) {
  if (is.null(filename_new)) {
    base <- sub("\\.csv$", "", filename_table, ignore.case = TRUE)
    filename_new <- sprintf("%s_new.csv", base)
  }
  if (file.exists(filename_new) && !overwrite) {
    stop(sprintf(
      "File '%s' exists; please remove to continue (or use overwrite = TRUE)",
      filename_new))
  }

  table <- read_csv(filename_table)
  msg <- setdiff(c("from", "to"), names(table))
  if (length(msg) > 0L) {
    stop("Missing columns in '%s': %s", filename_table,
         paste(msg, collapse = ", "))
  }
  dat <- recode_character_suggest(x, table, ...)

  if (!dat$all_matched) {
    message(sprintf("Matched %d / %d entries",
                    sum(!dat$is_unmatched), length(dat$is_unmatched)))
    s <- dat$suggestions
    write_csv(s, filename_new)

    message(sprintf("Suggestions added to '%s'", filename_new))
    message(sprintf("Copy these into '%s', delete '%s' and rerun",
                    filename_table, filename_new))
  }

  dat
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
