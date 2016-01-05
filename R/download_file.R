download_file <- function(url, dest=tempfile(), overwrite=FALSE, quiet=FALSE) {
  r <- filter_warnings(
    httr::GET(url,
              httr::write_disk(dest, overwrite),
              if (!quiet) httr::progress("down")),
    "progress callback must return boolean")

  if (!quiet) {
    cat("\n")
  }
  if (httr::status_code(r) >= 300L) {
    file.remove(dest)
    httr::stop_for_status(r)
  }
  dest
}

## Workaround for the CRAN version of httr + curl (development version
## gives suprious messages right now).
filter_warnings <- function(expr, pattern) {
  match_any <- function(x, pattern) {
    any(vapply(pattern, grepl, logical(1), x))
  }
  w <- function(w) {
    if (match_any(w$message, pattern)) {
      invokeRestart("muffleWarning")
    }
  }
  withCallingHandlers(expr, warning = w)
}
