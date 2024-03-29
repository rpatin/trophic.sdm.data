% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_sampling_effort.R
\docType{class}
\name{sampling_effort}
\alias{sampling_effort}
\alias{sampling_effort-class}
\alias{calc_sampling_effort}
\alias{show,sampling_effort-method}
\title{Object class containing summary of gbif sampling effort across 
taxonomy (Class or Order)}
\usage{
calc_sampling_effort(
  checklist,
  folder.gbif,
  sampling.effort.config,
  filter.atlas = FALSE,
  data.mask,
  project.name,
  nb.cpu = 1
)

\S4method{show}{sampling_effort}(object)
}
\arguments{
\item{checklist}{a \code{data.frame} with information on all species of interest}

\item{folder.gbif}{a \code{character}: folders in which gbif data are stored}

\item{sampling.effort.config}{a named \code{list} with one element per effort
layer to be calculated. Contains layer name as well as their associated taxa.}

\item{filter.atlas}{a \code{boolean}, whether atlas data should be filtered.
The method only exclude data with precision <10km in the gbif dataset. It is
not quite reliable as many atlas data are registered with a very high 
precision, despite describing squares of 25km side.}

\item{data.mask}{a \code{SpatRaster} with the extent and the grid used 
in the project}

\item{project.name}{a \code{character} indicating the folder in which logfiles 
and data may be written}

\item{nb.cpu}{number of CPU to use}

\item{object}{an object of class \code{sampling_effort}}
}
\description{
Class returned by \code{\link{calc_sampling_effort}}, 
that calculate gbif sampling effort across several taxa, depending on argument
\code{sampling.effort.config}.
}
\section{Slots}{

\describe{
\item{\code{data}}{a \code{SpatRaster} with several layers of sampling effort}

\item{\code{config}}{a \code{list} with the initial configuration of the sampling
effort calculation (taxonomic repartition)}

\item{\code{species.layer}}{a \code{list} with the layer name corresponding to each 
species}

\item{\code{checklist}}{a \code{data.frame} with information on all species of interest}

\item{\code{project.name}}{a \code{character} indicating the folder in which logfiles 
and data may be written}

\item{\code{filled}}{a \code{boolean}, whether or not the object is empty of filled 
with data}
}}

\examples{

showClass("sampling_effort")
}
\author{
Remi Patin
}
