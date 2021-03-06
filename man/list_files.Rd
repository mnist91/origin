% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_files.R
\name{list_files}
\alias{list_files}
\title{List all files in a directory by allow for excluding subdirectories}
\usage{
list_files(path, exclude_folders = c("renv", "packrat", "tests"), ...)
}
\arguments{
\item{path}{character string of path to list files from}

\item{exclude_folders}{character vector of folders to exlcude}

\item{...}{arguments passed to \link[base]{list.files}}
}
\value{
a character vector of file names
}
\description{
List all files in a directory by allow for excluding subdirectories
}
\examples{
\donttest{
list_files(".",
           exclude_folders = c("renv", "packrat", "tests"),
           pattern = "\\\\.R$",
           ignore.case = TRUE)
}
}
