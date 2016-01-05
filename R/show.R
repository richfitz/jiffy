##' Display a gif using shiny
##' @title Display a gif
##'
##' @param filename Filename of gif
##'
##' @param method One of "browser", "shiny" or "gadget".  If you have
##'   a recent version of shiny and are running in Rstudio, use method
##'   "gadget" to open the gif in the viewer window.
##'
##' @export
show_gif <- function(filename, method="browser") {
  if (!file.exists(filename)) {
    stop("File does not exist: ", filename)
  }
  filename <- normalizePath(filename)
  if (missing(method) && !requireNamespace("shiny", quietly=TRUE)) {
    method <- "browser"
  }
  method <- match.arg(method, c("shiny", "gadget", "browser"))
  if (method == "browser") {
    browse_gif(filename)
  } else {
    shiny_gif(filename, method == "gadget")
  }
}

shiny_gif <- function(filename, gadget) {
  loadNamespace("shiny")
  ui <- shiny::imageOutput("jiffy")
  server <- shiny::shinyServer(function(input, output, session) {
    output$jiffy <- shiny::renderImage({
      list(src = filename)
    }, deleteFile = FALSE)
  })

  version <- packageVersion("shiny")

  if (gadget && version <= numeric_version("0.12.2")) {
    warning(sprintf("No gadget support in shiny %s; please upgrade", version))
    gadget <- FALSE
  }
  if (gadget) {
    shiny::runGadget(ui, server)
  } else {
    shiny::runApp(list(ui=ui, server=server))
  }
}

## Because OSX uses "open" here, we can't just use browseURL because
## it will open in Preview.app
browse_gif <- function(filename) {
  page <- sprintf('<html><img src="%s"></html>', filename)
  dest <- tempfile()
  writeLines(page, dest)
  browseURL(dest)
}
