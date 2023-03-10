% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_border.R
\name{map_effort}
\alias{map_effort}
\title{Plot sampling effort}
\usage{
map_effort(project.name, vector_data, data.mask, suffix)
}
\arguments{
\item{project.name}{a \code{character} indicating the folder in which logfiles 
and data may be written}

\item{vector_data}{a \code{SpatVector} with administrative boundary}

\item{data.mask}{a \code{SpatRaster} with the extent and the grid used 
in the project}

\item{suffix}{a \code{character} with the name of the administrative boundary}
}
\description{
\code{plot_effort} represent the sampling effort based on 
administrative boundaries (country or regions)
}
\author{
Remi Patin
}
