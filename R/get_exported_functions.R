#' Get All Exported Functions From a Package
#'
#' @param pkg string of a package name
#'
#' @return character vector of functions names
#' @export
#'
#' @examples
#' get_exported_functions("data.table")
#'
get_exported_functions <- function(pkg) {
  # get all exported functions from a package --------------------------------
  # lists all exports of a package (incl. non functions)
  exports <- getNamespaceExports(pkg)

  is_function <- vapply(exports,
                        FUN = function(x, ns) {
                          inherits(x = getExportedValue(ns, x),
                                   what = "function")
                        },
                        FUN.VALUE = logical(1),
                        ns = getNamespace(pkg))

  # overlap of exports and functions -> exported functions
  functions <- exports[is_function]

  return(functions)
}
