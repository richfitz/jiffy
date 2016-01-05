##' Search the giphy archives.
##'
##' If \code{n} is omitted, giphy will return 25 images.
##'
##' Note that all the \code{giphy_api_*} functions do not actually
##' download gifs.
##' @title Search the giphy archives
##' @param phrase Phrase to search for (required)
##' @param n Maximum number of images to return.
##' @param offset Starting point in the stream to search from (by
##'   default, starts at the beginning of the stream, but specify 50
##'   to start at the 50th gif).
##' @param rating Limits gifs to a rating of y, g, pg, pg-13 or r
##'   (this is a limit not a filter).
##' @param type Either "gifs" or "stickers" (or a partial string match
##'   to either).  The default is to return gifs.
##' @export
##' @return A list of results that is a little complicated at the
##'   present.
##' @author Rich FitzJohn
giphy_api_search <- function(phrase, n=NULL, offset=NULL, rating=NULL,
                         type="gifs") {
  query <- list(q=gsub(" ", "+", phrase))
  query$offset <- offset
  query$rating <- rating
  res <- giphy_api_request(type, "search", query, n=n)

  ret <- lapply(res$data, giphy_img)
  attr(ret, "total") <- res$pagination$total_count
  ret
}

##' Intelligently translate phrases into gifs (in the way used by
##' Slack's \code{/giphy} add-in).  Giphy adds some "special sauce" to
##' make the results more relevant.
##'
##' Note that all the \code{giphy_api_*} functions do not actually
##' download gifs.
##' @title Translate words to gifs
##' @param phrase Search word or phrase
##' @inheritParams giphy_api_search
##' @return A list of results that is a little complicated at the moment
##' @export
##' @author Rich FitzJohn
giphy_api_translate <- function(phrase, rating=NULL, type="gifs") {
  query <- list(s=gsub(" ", "+", phrase))
  query$rating <- rating
  giphy_img(giphy_api_request(type, "translate", query)$data)
}

##' Retrieve a random gif, by tag.
##'
##' Note that all the \code{giphy_api_*} functions do not actually
##' download gifs.
##' @title Retrieve a random gif, by tag.
##' @param tag The gif tag (much narrower set of allowed words than
##'   \code{phrase} in \code{\link{giphy_api_search}} and
##'   \code{\link{giphy_api_translate}}.
##' @inheritParams giphy_api_search
##' @export
##' @author Rich FitzJohn
giphy_api_random <- function(tag=NULL, rating=NULL, type="gifs") {
  if (is.null(tag)) {
    query <- list()
  } else {
    query <- list(tag=gsub(" ", "+", tag))
  }
  query$rating <- rating
  x <- giphy_api_request(type, "random", query)
  giphy_api_id(x$data$id, type)[[1]]
}

##' Retrieve trending gifs.
##'
##' Note that all the \code{giphy_api_*} functions do not actually
##' download gifs.
##' @title Retrieve trending gifs
##' @inheritParams giphy_api_search
##' @export
giphy_api_trending <- function(n=25, rating=NULL, type="gifs") {
  if (!is.null(n) && !is.finite(n)) {
    stop("n must be finite for trending")
  }
  query <- list()
  query$limit <- min(100, n)
  query$rating <- rating
  res <- giphy_api_request(type, "trending", query, n=n)

  lapply(res$data, giphy_img)
}

##' Retrieve gif by id.  Less useful in general because you need to
##' know the id.  But this is useful for getting a specific gif.
##'
##' Note that all the \code{giphy_api_*} functions do not actually
##' download gifs.
##' @title Retrieve gif by id
##' @param ids A vector of ids
##' @inheritParams giphy_api_search
##' @export
giphy_api_id <- function(ids, type="gifs") {
  res <- giphy_api_request(type, paste(ids, collapse=","), NULL)
  if (length(ids) == 1L) {
    list(giphy_img(res$data))
  } else {
    lapply(res$data, giphy_img)
  }
}

giphy_api_request <- function(type, endpoint, query, n=NULL) {
  m <- 0L
  do_request <- function(url, query) {
    if (!is.null(n)) {
      query$limit <- min(100, n - m)
    }
    r <- httr::GET(url, query=query)
    httr::stop_for_status(r)
    httr::content(r)
  }
  url <- file.path("http://api.giphy.com/v1", giphy_api_type(type))
  if (!is.null(endpoint)) {
    url <- file.path(url, endpoint)
  }
  query <- c(query, giphy_api_auth())
  res <- do_request(url, query)
  m <- length(res$data)
  while (giphy_api_continue(res, n, m)) {
    query$offset <- res$pagination$count + res$pagination$offset
    res2 <- do_request(url, query)
    res$data <- c(res$data, res2$data)
    res$pagination <- res2$pagination
    m <- length(res$data)
  }
  res
}

giphy_api_type <- function(type) {
  match.arg(type, c("gifs", "stickers"))
}

giphy_api_continue <- function(res, n, m) {
  total <- res$pagination$total_count
  if (is.null(res$pagination$total_count)) {
    total <- Inf
  }
  !is.null(n) && !is.null(res$pagination) && m < total && m < n
}

giphy_api_auth <- function() {
  key <- Sys.getenv("GIPHY_API_KEY", "dc6zaTOxFJmzC")
  list(api_key=key)
}

giphy_img <- function(x) {
  ret <- x
  tr <- function(el) {
    int <- names(el) %in% c("width", "height", "frames",
                            "size", "mp4_size", "webp_size")
    if (any(int)) {
      el[int] <- lapply(el[int], as.integer)
    }
    el
  }
  ret$images <- lapply(ret$images, tr)
  copy <- c("width", "height", "frames", "size")
  ret[copy] <- ret$images$original[copy]
  class(ret) <- "giphy"
  ret
}
