% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_trophic_dataset.R
\docType{class}
\name{trophic_species}
\alias{trophic_species}
\alias{trophic_species-class}
\alias{show,trophic_species-method}
\title{Status of each species within a \code{\link{trophic_dataset}}}
\usage{
\S4method{show}{trophic_species}(object)
}
\arguments{
\item{object}{an object of class \code{trophic_species}}
}
\description{
Class contained within \code{\link{trophic_dataset}}. A
  \code{trophic_species} object contains a set of information on the different
  species that went through the workflow:
  \itemize{
  \item the species kept in the final dataset
  \item the species filtered out through the workflow
  \item the prey filtered out for each predator
  \item the method used for each species kept (IUCN or gbif)
  \item whether data was freshly extracted or reused from a previous extraction
  }
}
\section{Slots}{

\describe{
\item{\code{kept}}{a named \code{character} vector with all species kept and the method used}

\item{\code{filtered}}{a named \code{character} vector with all species removed 
as well as the motivation of removal}

\item{\code{kept.prey}}{a named \code{list} with the prey kept for each species}

\item{\code{filtered.prey}}{a named \code{list} vector with the prey removed for 
each species as well as the motivation of removal}

\item{\code{fresh.occurrence}}{a named \code{logical} vector. For each species
whether occurrence extraction was freshly done or not}

\item{\code{fresh.trophic}}{a named \code{logical} vector. For each species whether
trophic extraction was freshly done or not}
}}

\author{
Remi Patin
}
