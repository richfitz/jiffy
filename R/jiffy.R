##' Download a random gif from giphy.
##'
##' @title Download a random gif from giphy
##' @param tag An optional tag to search on.  If omitted then you git
##'   a random image pulled from the giphy's trending stream (I
##'   believe).
##' @param phrase A phrase to search for
##' @param type Either "gifs" or "stickers" (or a partial string match
##'   to either).  The default is to return gifs.
##' @inheritParams fetch
##' @return A filename of the downloaded file.
##' @rdname jiffy
##' @examples
##' \dontrun{
##' # Download a random cat:
##' gif <- jiffy_random("cat")
##' show_gif(gif)
##'
##' # MRW someone suggests not using stringsAsFactors=FALSE
##' gif <- jiffy_translate("hell no")
##' show_gif(gif)
##' }
jiffy_random <- function(tag=NULL, type="gifs", what="original", format="gif",
                         dest=NULL, quiet=FALSE) {
  res <- giphy_api_random(tag, type)
  fetch(res, what, format, dest, quiet)
}

##' @export
##' @rdname jiffy
jiffy_translate <- function(phrase, type="gifs", what="original", format="gif",
                            dest=NULL, quiet=FALSE) {
  res <- giphy_api_translate(phrase, type)
  fetch(res, what, format, dest, quiet)
}

##' @export
print.giphy <- function(x, ...) {
  f <- function(x, name) {
    if (is.null(x[[name]])) NA else x[[name]]
  }
  g <- function(x) {
    ifelse(is.na(x), "?", x)
  }
  w <- vapply(x$images, FUN.VALUE=integer(1), f, "width")
  h <- vapply(x$images, FUN.VALUE=integer(1), f, "height")
  b <- vapply(x$images, FUN.VALUE=integer(1), f, "size") / 1000
  bs <- format(b, justify="right", digits=1, nsmall=2)
  bs[is.na(b)] <- "?"

  ord <- order(w, names(w), decreasing=TRUE)
  d <- cbind(name=names(w), w=g(w), h=g(h), b=bs, still="")[ord, ]

  nms <- d[, "name"]

  re <- "_still$"
  i <- grepl(re, nms)
  j <- match(sub(re, "", nms[i]), nms)
  k <- !is.na(j) # defensive

  d[j[k], "still"] <- " (+ _still)"
  d <- d[-which(i)[k], ]

  imgs <- sprintf("  - %s %s x %s, %s Kb%s\n",
                  format(paste0(d[, "name"], ":")),
                  format(d[, "w"], justify="right"),
                  format(d[, "h"], justify="right"),
                  format(d[, "b"], justify="right"),
                  d[, "still"])

  cat(sprintf("<gif %s> [%s x %s, %d frames, %2.3f Kb]\n  %s\n",
              x$id, x$width, x$height, x$frames, x$size / 1000,
              x$images$original$url))
  cat(paste(imgs, collapse=""))
  invisible(x)
}

##' Download a gif
##' @title Download a gif
##' @param x A giphy object
##' @param what The type of image to download.  \code{original} will
##'   always be available but giphy supplies \code{fixed_height},
##'   \code{fixed_width} \code{downsized}, etc.  See
##'   \code{names(x$images)} for possible options for a given image.
##'   The default is to download the original (largest) gif.
##' @param format The format to download in.  Default is gif, but
##'   \code{mp4} and \code{webp} are often available (for
##'   \code{what="looping"} the only option is \code{mp4}.
##' @param dest Destination to download the file to.  Default is a
##'   temporary file.
##' @param quiet Logical indicating if progress bars during download
##'   should be suppressed.
##' @return The filename of the downloaded file.  This can be fed into
##'   \code{\link{show_gif}}.
##' @author Rich FitzJohn
fetch <- function(x, what="original", format="gif",
                  dest=NULL, quiet=FALSE) {
  if (!inherits(x, "giphy")) {
    stop("Expected a 'giphy' object")
  }
  if (is.null(dest)) {
    dest <- tempfile(fileext=paste0(".", format))
  }
  el <- if (format == "gif") "url" else format
  url <- x$images[[what]][[el]]
  if (is.null(url)) {
    stop(sprintf("No download possible for %s / %s", what, format))
  }
  download_file(url, dest, quiet)
}
