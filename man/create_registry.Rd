% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_orchestration.R
\name{create_registry}
\alias{create_registry}
\title{Creates Registry (csv file) for running a process on a batch of files}
\usage{
create_registry(output_dir, source_files)
}
\arguments{
\item{output_dir}{the path to contain the process registry file.}

\item{source_files}{the list of files to be processed.}
}
\value{
the path to the process registry file.
}
\description{
Creates Registry (csv file) for running a process on a batch of files
}
\examples{
create_registry('./data/L2/demo/', './data/L1/subject_01/response.csv')
}
