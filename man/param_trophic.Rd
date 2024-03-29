% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_trophic_dataset.R
\docType{class}
\name{param_trophic}
\alias{param_trophic}
\alias{param_trophic-class}
\alias{show,param_trophic-method}
\title{Parameters of a trophic_dataset}
\usage{
\S4method{show}{param_trophic}(object)
}
\arguments{
\item{object}{an object of class \code{param_trophic}}
}
\description{
Class contained within \code{\link{trophic_dataset}}. A
  \code{param_trophic} object contains the set of parameters used to 
  generate the dataset:
  \itemize{
  \item the final and raw checklist of species 
  \item the final and raw metaweb 
  \item the set of parameters used for gbif extraction
  \item the folder with IUCN ranges
  \item the set of parameters used for subsampling
  }
}
\section{Slots}{

\describe{
\item{\code{checklist}}{a \code{data.frame} with information on all species 
that remain after filtering of species.}

\item{\code{metaweb}}{a \code{data.frame} with all species interactions 
that remain after filtering of species and prey.}

\item{\code{trophic.groups}}{a \code{data.frame}}

\item{\code{checklist.raw}}{a \code{data.frame} with original information on all
species}

\item{\code{metaweb.raw}}{a \code{data.frame} with all original species interactions}

\item{\code{trophic.groups.raw}}{a \code{data.frame}}

\item{\code{param.gbif}}{a \code{\link{param_gbif}} object with all parameters used
to extract gbif data}

\item{\code{param.subsampling}}{a \code{\link{param_subsampling}} object with all
parameters used for subsampling}

\item{\code{folder.iucn}}{a \code{character}, folder in which IUCN range
are stored as raster files.}
}}

\author{
Remi Patin
}
