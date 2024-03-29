% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pkgs_from_description.R
\name{get_pkgs_from_description}
\alias{get_pkgs_from_description}
\title{Get Packages from the DESCRIPTION file}
\usage{
get_pkgs_from_description(path = NULL)
}
\arguments{
\item{path}{Path to a DESC>RIPTION file, If `NULL` (default), the functions
searches for a description file in the current active project}
}
\value{
character vector of package names
}
\description{
It looks for a DESCRIPTION file in the current project and
returns all packages listed in Suggests, Imports, and Depends.
}
\examples{
# Only works inside of a package developing project
\dontrun{
get_pkgs_from_description()
}
}
