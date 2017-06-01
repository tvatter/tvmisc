
#' Return a source code with roxygen comments
#'
#' @param input input filename.
#' @param start starting character in each line.
#' @param output output filename.
#'
#' @author Thibault Vatter
#'
#' @export
roxygen.comment <- function(input, start=1, output=input){
  rox <- sapply(readLines(input), function(x)
    paste("#'",substring(x,start)))
  writeLines(rox, output)
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
