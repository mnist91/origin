# DEV ONLY!
# the current rstudio version lacks a bug in its sourceMarkers function.
# Until it is fixed I mimic the expected fix.
# Note: using unlock binding is generally highly discouraged!

my_callFun <- function (fname, ...)
{
  if (isChildProcess())
    return(callRemote(sys.call(), parent.frame()))
  verifyAvailable()
  if (usingTools())
    found <- exists(toolsName(fname), envir = toolsEnv(),
                    mode = "function")
  else found <- exists(fname, envir = asNamespace("rstudio"),
                       mode = "function")
  if (!found)
    stop("Function ", fname, " not found in RStudio", call. = FALSE)
  # USE OWN FUNCTION
  # f <- findFun(fname, mode = "function")
  f <- my_marker_function
  args <- list(...)
  if (!"..." %in% names(formals(f))) {
    while (length(args) > length(formals(f))) args <- args[-length(args)]
  }
  do.call(f, args)
}


my_marker_function <- function(name,
                               markers,
                               basePath = NULL,
                               autoSelect = c("none", "first", "error")) {
  if (!is.character(name))
    stop("name parameter is specified or invalid: ", name,
         call. = FALSE)
  autoSelect = match.arg(autoSelect)
  if (!is.null(basePath))
    basePath <- .rs.normalizePath(basePath, mustWork = TRUE)
  if (is.data.frame(markers)) {
    cols <- colnames(markers)
    if (!"type" %in% cols || !is.character(markers$type))
      stop("markers type field is unspecified or invalid",
           call. = FALSE)
    if (!"file" %in% cols || !is.character(markers$file))
      stop("markers file field is unspecified or invalid",
           call. = FALSE)
    if (!"line" %in% cols || !is.numeric(markers$line))
      stop("markers line field is unspecified or invalid",
           call. = FALSE)
    if (!"column" %in% cols || !is.numeric(markers$column))
      stop("markers column field is unspecified or invalid",
           call. = FALSE)
    if (!"message" %in% cols || !is.character(markers$message))
      stop("markers message field is unspecified or invalid",
           call. = FALSE)
    markers$file <- .rs.normalizePath(markers$file, mustWork = TRUE)
    markers$messageHTML <- inherits(markers$message, "html")
  }
  else if (is.list(markers)) {
    markers <- lapply(markers, function(marker) {
      markerTypes <- c("error", "warning", "box", "info",
                       "style", "usage")
      if (is.null(marker$type) || (!marker$type %in% markerTypes))
        stop("Invalid marker type (", marker$type, ")",
             call. = FALSE)
      if (!is.character(marker$file))
        stop("Marker file is unspecified or invalid: ",
             marker$file, call. = FALSE)
      if (!is.numeric(marker$line))
        stop("Marker line is unspecified or invalid",
             marker$line, call. = FALSE)
      if (!is.numeric(marker$column))
        stop("Marker column is unspecified or invalid",
             marker$line, call. = FALSE)
      if (!is.character(marker$message))
        stop("Marker message is unspecified or invalid: ",
             marker$message, call. = FALSE)
      marker$type <- .rs.scalar(marker$type)
      marker$file <- .rs.scalar(.rs.normalizePath(marker$file,
                                                  mustWork = TRUE))
      marker$line <- .rs.scalar(as.numeric(marker$line))
      marker$column <- .rs.scalar(as.numeric(marker$column))

      # LINE ORDER SWITCH
      marker$messageHTML <- .rs.scalar(inherits(marker$message,
                                                "html"))
      marker$message <- .rs.scalar(marker$message)
      marker
    })
  }
  else {
    stop("markers was not a data.frame or a list", call. = FALSE)
  }
  if (is.null(basePath))
    basePath <- ""
  else if (!is.character(basePath))
    stop("basePath parameter is not of type character",
         call. = FALSE)
  invisible(.Call("rs_sourceMarkers", name, markers, basePath,
                  autoSelect))
}

unlockBinding("callFun", env = asNamespace("rstudioapi"))
assignInNamespace(x = "callFun", value = my_callFun, ns = asNamespace("rstudioapi"))
