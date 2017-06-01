
#' Apply roxygen comments
#' 
#' @details If all lines 'touched' by the cursor or selection start with a 
#' roxygen2 comment, then the method will remove it. Otherwise, it will add the 
#' roxygen2 comment. In the case there is multiple cursors/selection only 
#' the first one (the primary one) will be considered.
#'
#' @author Thibault Vatter
#'
#' @export
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' primary_selection modifyRange
roxygen.comment <- function() {

  # Capture the selection
  context <- getActiveDocumentContext()
  selection <- primary_selection(context)
  
  # Involved rows
  row_range <- selection$range$start[['row']]:selection$range$end[['row']]
  
  # If roxygen2 block remove #', otherwise add #'
  is_roxy <- sapply(context$contents[row_range], 
                    function(x) substr(x, 1L, 2L) == "#'")
  if(all(is_roxy)) {
    rng <- Map(function(x,y,z) c(x,y,x,z), row_range, 1, 4)
    modifyRange(rng, "")
  } else {
    pos <- Map(c, row_range, 1)
    insertText(pos, "#' ")
  }
}

#' Soure a directory
#'
#' @param path directory to source
#' @param trace print informations
#' @param recursive see \code{\link{list.files}}
#' @param ... additional arguments passed to \code{\link{source}}
#'
#' @author Thibault Vatter
#'
#' @export
sourceDir <- function(path=getwd(), trace = TRUE, recursive = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$", recursive =
                        recursive)) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

#' Improved list of objects
#'
#' @param pos where to look for (see \code{\link{get}} and \code{\link{ls}})
#' @param pattern an optional regular expression (see \code{\link{ls}})
#' @param order.by one of `c("Type", "Size", "PrettySize", "Rows", "Columns")`
#' @param decreasing \code{logical} (for `order.by`)
#' @param head returns the first part of the list
#' @param n number of objects if `head=TRUE`
#'
#' @author http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#'
#' @export
#' @aliases lsos
ls_objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

#' @export
#' @rdname ls_objects
lsos <- function(..., n=10) {
  ls_objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

#' List functions in a package
#'
#' @param package the package name
#' @param all.names a logical value (see \code{\link{ls}}). If `TRUE`, all
#' object names are returned. If `FALSE`, names which begin with a `.` are
#' omitted.
#' @param pattern an optional regular expression (see \code{\link{ls}})
#'
#' @author Thibault Vatter
#'
#' @export
lsp <- function(package, all.names = FALSE, pattern)
{
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}
