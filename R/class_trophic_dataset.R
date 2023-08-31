## --------------------------------------------------------------------------- #
# 1. trophic_summary         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name trophic_summary
##' @aliases trophic_summary-class
##' @author Remi Patin
##'
##' @title Summary of trophic dataset
##'
##' @description Class contained within \code{\link{trophic_dataset}}. A
##'   \code{trophic_summary} object contains a set of summary \code{data.frame} 
##'   for the \code{\link{trophic_dataset}} object:
##'   \itemize{
##'   \item occurrence summary
##'   \item final dataset summary
##'   \item prey summary
##'   \item filtered species summary
##'   \item patrimonial and protected species summary
##'   }
##'
##' @slot occurrence a \code{data.frame} with the list of species and 
##' the total number of presence and absence in the occurrence dataset extracted
##' from gbif and independently of prey cell selection.
##' @slot trophic a \code{data.frame} with a summary of the actual 
##' trophic dataset, i.e. list of species and the total number of presence 
##' and absence inside/outside IUCN range as well as additional information 
##' such as the number of prey.
##' @slot prey a \code{data.frame} with a summary of occurrence and 
##' prevalence for all prey associated to a predator dataset.
##' @slot filtered a \code{data.frame} with a summary of species that were 
##' filtered throughout the data generation
##' @slot protected a \code{data.frame} with a summary of species protected by 
##' the EU habitat directive, listed in IUCN red list or within french patrimonial
##' species list

## 1.1 Class Definition ----------------------------------------------------------------------------

setClass("trophic_summary",
         representation(occurrence = "data.frame",
                        trophic = "data.frame",
                        prey = "data.frame",
                        filtered = "data.frame",
                        protected = "data.frame"),
         validity = function(object){
           TRUE
         })


## 1.2 Methods -------------------------------------------------------------
### show.trophic_summary    --------------------------------------------------
##'
##' @rdname trophic_summary
##' @importMethodsFrom methods show
##' @param object an object of class \code{trophic_summary}
##' @importFrom cli cli_h2 cli_h3 cli_li cli_text
##' @export
##'

setMethod('show', signature('trophic_summary'),
          function(object)
          {
            n.species <- nrow(object@occurrence)
            if (n.species == 0) {
              cli_alert_warning("Empty trophic_summary object")
            } else {
              n.trophic <- nrow(object@trophic)
              n.prey <- nrow(object@prey)
              n.filtered <- nrow(object@filtered)
              n.protected <- nrow(object@protected)
              cli_h3("trophic_summary object")
              cli_text("Summary data.frame for:")
              cli_li("Occurrence data for {n.species} species")
              cli_li("Trophic data for {n.trophic} species")
              cli_li("{n.prey} predator-prey interactions")
              cli_li("{n.filtered} removed species")
              cli_li("{n.protected} protected, endangered or patrimonial species")
            }
            
            invisible(NULL)
          })

## --------------------------------------------------------------------------- #
# 2. trophic_files         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name trophic_files
##' @aliases trophic_files-class
##' @author Remi Patin
##'
##' @title Set of links to data files
##'
##' @description Class contained within \code{\link{trophic_dataset}}. A
##'   \code{trophic_files} object contains a set of file links to the different
##'   dataset generated throughout the function \code{\link{prepare_dataset}}:
##'   \itemize{
##'   \item Occurrence files (focal species only)
##'   \item Occurrence files (as \code{\link{SpatRaster}})
##'   \item Status files (as \code{\link{SpatRaster}})
##'   \item Raw trophic files (focal species and its prey)
##'   \item Final trophic files (final subsampled dataset)
##'   }
##'
##' @slot occurrence a named \code{vector} with a path to raw 
##' occurrence data for each species.
##' @slot occurrence.rast a named \code{vector} with a path to raw raster 
##' occurrence data for each species.
##' @slot status.rast a named \code{vector} with a path to raster data 
##' containing status of data (certain vs uncertain) for each species.
##' @slot trophic a named \code{vector} with a path to filtered
##' trophic data for each species.
##' @slot trophic.raw a named \code{vector} with a path to raw
##' trophic data for each species.

## 2.1 Class Definition ----------------------------------------------------------------------------

setClass("trophic_files",
         representation(occurrence = "character",
                        occurrence.rast = "character",
                        status.rast = "character",
                        trophic = "character",
                        trophic.raw = "character"),
         validity = function(object){
           stopifnot(!is.null(names(object@occurrence)))
           stopifnot(!is.null(names(object@trophic)))
           stopifnot(!is.null(names(object@trophic.raw)))
           TRUE
         })

## 2.2 Methods -------------------------------------------------------------
### show.trophic_files    --------------------------------------------------
##'
##' @rdname trophic_files
##' @importMethodsFrom methods show
##' @param object an object of class \code{trophic_files}
##' @importFrom cli cli_h2 cli_h3 cli_li cli_text
##' @export
##'

setMethod('show', signature('trophic_files'),
          function(object)
          {
            n.species <- length(object@occurrence)
            if (length(n.species) == 0) {
              cli_alert_warning("Empty trophic_files object")
            } else {
              n.trophic <- length(object@trophic)
              n.trophic.raw <- length(object@trophic.raw)
              cli_h3("trophic_files object")
              cli_text("Available file links for:")
              cli_li("Occurrence data for {n.species} species")
              cli_li("Raw trophic data for {n.trophic.raw} species")
              cli_li("Subsampled trophic data for {n.trophic} species")
            }
            invisible(NULL)
          })

## --------------------------------------------------------------------------- #
# 3. backup_iucn         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name backup_iucn
##' @aliases backup_iucn-class
##' @author Remi Patin
##'
##' @title Parameters for IUCN backup
##'
##' @description Class contained within a \code{\link{param_gbif}} object. A
##'   \code{iucn_backup} object contains the set of parameters used to 
##'   decide whether gbif data should be dropped to use iucn data instead:
##'   \itemize{
##'   \item whether backup was activated or not
##'   \item a minimum number of gbif occurrences 
##'   \item a minimum sampling effort threshold
##'   \item a maximum proportion of uncertain cells within its own range
##'   \item a maximum proportion of uncertain cells within the predator IUCN range 
##'   \item the quantile to aggregate proportion accross predator
##'   }
##'
##' @slot do.backup a \code{logical} whether backup was activated or not
##' @slot min.occurrence a \code{integer}, the minimum number of gbif occurrences
##' @slot min.threshold.effort a \code{integer}, the minimum sampling effort
##'   threshold
##' @slot max.prop.own a \code{numeric}, the maximum proportion of uncertain
##'   cells within its own range
##' @slot max.prop.predator  a \code{numeric}, the maximum proportion of
##'   uncertain cells within its predators range
##' @slot max.prop.predator.outside  a \code{numeric}, the maximum proportion of
##'   uncertain cells outside its predators range
##' @slot quantile.prop.predator a \code{numeric}, used to aggregate information
##' accross several predators.


## 3.1 Class Definition ----------------------------------------------------------------------------

setClass("backup_iucn",
         representation(do.backup = "logical",
                        min.occurrence = "numeric",
                        min.threshold.effort = "numeric",
                        max.prop.own = "numeric",
                        max.prop.predator = "numeric",
                        max.prop.predator.outside = "numeric",
                        quantile.prop.predator = "numeric"),
         validity = function(object){
           if(object@do.backup){
             .fun_testIfPosInt(object@min.occurrence)
             .fun_testIfPosInt(object@min.threshold.effort)
             .fun_testIf01(object@max.prop.own)
             .fun_testIf01(object@max.prop.predator)
             .fun_testIf01(object@max.prop.predator.outside)
             .fun_testIf01(object@quantile.prop.predator)
           }
           TRUE
         })

## 3.2 Constructor ------------------------------------------------------------
##' 
##' @rdname backup_iucn
##' @export
##' @param do.backup a \code{logical} whether backup was activated or not
##' @param min.occurrence a \code{integer}, the minimum number of gbif occurrences
##' @param min.threshold.effort a \code{integer}, the minimum sampling effort
##'   threshold
##' @param max.prop.own a \code{numeric}, the maximum proportion of uncertain
##'   cells within its own range
##' @param max.prop.predator  a \code{numeric}, the maximum proportion of
##'   uncertain cells within its predators range
##' @param max.prop.predator.outside  a \code{numeric}, the maximum proportion of
##'   uncertain cells outside its predators range
##' @param quantile.prop.predator a \code{numeric}, used to aggregate information
##' accross several predators.
##' @importFrom methods validObject

set_backup_iucn <- function(do.backup,
                            min.occurrence,
                            min.threshold.effort,
                            max.prop.own,
                            max.prop.predator,
                            max.prop.predator.outside,
                            quantile.prop.predator) {
  
  out <- new("backup_iucn")
  cli_h3("Setup IUCN backup")
  
  # do.backup deactivated by default
  if (missing(do.backup)) {
    cli_alert_info("IUCN backup deactivated")
    do.backup <- FALSE
  }
  out@do.backup <- do.backup
  
  # min.occurrence
  if (missing(min.occurrence)) {
    if (do.backup) cli_alert_info("Minimum number of occurences set to 25")
    out@min.occurrence <- 25
  } else {
    out@min.occurrence <- min.occurrence
  }
  
  # min.threshold.effort
  if (missing(min.threshold.effort)) {
    out@min.threshold.effort <- 2
    if (do.backup) cli_alert_info("Minimum effort threshold set to 2")
  } else {
    out@min.threshold.effort <- min.threshold.effort
  }
  
  # max.prop.own
  if (missing(max.prop.own)) {
    if (do.backup) cli_alert_info("Max proportion of uncertain whithin its range set to 99%")
    out@max.prop.own <- 0.99
  } else {
    out@max.prop.own <- max.prop.own
  }
  
  # max.prop.predator
  if (missing(max.prop.predator)) {
    if (do.backup) cli_alert_info("Max proportion of uncertain whithin its  predators\\
                               range set to 99%")
    out@max.prop.predator <- 0.99
  } else {
    out@max.prop.predator <- max.prop.predator
  }
  
  # max.prop.predator.outside
  if (missing(max.prop.predator.outside)) {
    if (do.backup) cli_alert_info("Max proportion of uncertain outside its  predators\\
                               range set to 99%")
    out@max.prop.predator.outside <- 0.99
  } else {
    out@max.prop.predator.outside <- max.prop.predator.outside
  }
  
  # quantile.prop.predator
  if (missing(quantile.prop.predator)) {
    out@quantile.prop.predator <- 1
    if (do.backup) cli_alert_info("Quantile used to aggregate predator information\\
                               set to 100% (maximum value)")
  } else {
    out@quantile.prop.predator <- quantile.prop.predator
  }
  
  validObject(out)
  
  out
}

## 3.3 Methods -------------------------------------------------------------
### show.backup_iucn    --------------------------------------------------
##'
##' @rdname backup_iucn
##' @importMethodsFrom methods show
##' @param object an object of class \code{backup_iucn}
##' @importFrom cli cli_h2 cli_h3 cli_li cli_text
##' @export
##'

setMethod('show', signature('backup_iucn'),
          function(object)
          {
            if (!object@do.backup) {
              cli_alert_warning("No IUCN Backup")
            } else {
              cli_h3("IUCN Backup Configuration")
              cli_text("gbif data are replace by IUCN data for species with:")
              cli_li("gbif rasterized occurrences are < {object@min.occurrence}")
              cli_li("threshold to consider absences as certain < {object@min.threshold.effort}")
              cli_li("proportion of uncertain absences within IUCN range > {object@max.prop.own}")
              cli_li("proportion of uncertain absences within its predator IUCN range > {object@max.prop.predator}")
              cli_li("proportion of uncertain absences outside its predator IUCN range > {object@max.prop.predator.outside}")
              cli_alert_info("quantile to aggregate predator information = {object@quantile.prop.predator}")
            }
            invisible(NULL)
          })


## --------------------------------------------------------------------------- #
# 4. param_gbif         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name param_gbif
##' @aliases param_gbif-class
##' @author Remi Patin
##'
##' @title Parameters for gbif data extraction
##'
##' @description Class contained within a \code{\link{param_trophic}} object. A
##'   \code{param_gbif} object contains the set of parameters used to 
##'   extract gbif occurrences. It contains:
##'  \itemize{
##'   \item whether gbif data were ever used
##'   \item the folder with gbif data
##'   \item the minimum number of gbif occurrences 
##'   \item whether atlas data should be filtered out (through column 
##'   \code{coordinateUncertaintyInMeters})
##'   \item The configuration for iucn buffer
##'   \item The folder with buffered IUCN ranges
##'   \item The quantile of sampling effort below which a species absence is
##'   considered as uncertain
##'   \item The minimum proportion of prey occurrence considered as certain for
##'   a raster cell to be included in the species dataset.
##'   \item The value given to uncertain absences in cells
##'   where more than \code{prop.prey.certain} prey are considered as certain.
##'   \item The parameter for IUCN backup (whether IUCN data should be used when
##'   gbif data are not sufficient)
##'   }
##'
##' @slot use.gbif a \code{logical}, whether gbif was used or not
##' @slot folder.gbif a \code{character}: folders in which gbif data are stored
##' @slot min.gbif a \code{integer}, the minimum number of gbif occurrences
##' @slot filter.atlas a \code{booelan}, whether atlas data should be filtered
##'  out (through column \code{coordinateUncertaintyInMeters})
##' @slot buffer.config a \code{list}, the configuration for iucn buffer
##' @slot species.buffer a named \code{list} with the buffer associated to each species
##' @slot folder.iucn.buffer  a \code{character}, folder in which IUCN range
##' are stored as raster files.
##' @slot quantile.absence.certain a \code{numeric} (\emph{default}
##'   \code{0.1}), between 0 and 1. The quantile of sampling effort below which
##'   a species absence is considered as uncertain
##' @slot prop.prey.certain a \code{numeric} (\emph{default} \code{0.8}),
##'   between 0 and 1. The minimum proportion of prey occurrence considered as
##'   certain for a raster cell to be included in the species dataset.
##' @slot uncertain.value a \code{integer} (\code{0} or \code{1} ;
##'   \emph{default} \code{0}). The value given to uncertain absences in cells
##'   where more than \code{prop.prey.certain} prey are considered as certain.
##' @slot backup.iucn a \code{\link{backup_iucn}} object
##' @slot sampling.effort a \code{sampling_effort} object describing the sampling
##' effort associated to all taxa.


## 4.1 Class Definition ----------------------------------------------------------------------------

setClass("param_gbif",
         representation(
           use.gbif = "logical",
           folder.gbif = "character",
           min.gbif = "numeric",
           filter.atlas = "logical",
           buffer.config = "list",
           species.buffer = "list",
           folder.iucn.buffer = "character",
           sampling.effort = "sampling_effort",
           quantile.absence.certain = "numeric",
           prop.prey.certain = "numeric",
           uncertain.value = "numeric",
           backup.iucn = "backup_iucn"),
         validity = function(object){
           .fun_testIfDirExists(object@folder.gbif)
           .fun_testIfPosInt(object@min.gbif)
           .fun_testIf01(object@quantile.absence.certain)
           .fun_testIf01(object@prop.prey.certain)
           .fun_testIfIn(object@uncertain.value, c(0,1))
           .fun_testIfPosNum(unlist(object@buffer.config))
           .fun_testIfInherits(names(object@buffer.config), "character")
           .fun_testIfDirExists(object@folder.iucn.buffer)
           TRUE
         })



## 4.2 Constructor ------------------------------------------------------------
##' 
##' @rdname param_gbif
##' @param ... additionnal parameters given to \code{\link{set_backup_iucn}}
##' @param use.gbif a \code{logical}, whether gbif was used or not
##' @param folder.gbif a \code{character}: folders in which gbif data are stored
##' @param min.gbif a \code{integer}, the minimum number of gbif occurrences
##' @param filter.atlas a \code{booelan}, whether atlas data should be filtered
##'  out (through column \code{coordinateUncertaintyInMeters})
##' @param buffer.config a \code{list}, the configuration for iucn buffer
##' @param folder.iucn.buffer  a \code{character}, folder in which IUCN range
##' are stored as raster files.
##' @param quantile.absence.certain a \code{numeric} (\emph{default}
##'   \code{0.1}), between 0 and 1. The quantile of sampling effort below which
##'   a species absence is considered as uncertain
##' @param prop.prey.certain a \code{numeric} (\emph{default} \code{0.8}),
##'   between 0 and 1. The minimum proportion of prey occurrence considered as
##'   certain for a raster cell to be included in the species dataset.
##' @param uncertain.value a \code{integer} (\code{0} or \code{1} ;
##'   \emph{default} \code{0}). The value given to uncertain absences in cells
##'   where more than \code{prop.prey.certain} prey are considered as certain.
##' @param backup.iucn a \code{\link{backup_iucn}} object
##' @param sampling.effort a \code{sampling_effort} object describing the sampling
##' effort associated to all taxa.
##' @importFrom methods validObject
##' @export

set_param_gbif <- function(use.gbif,
                           folder.gbif,
                           min.gbif,
                           filter.atlas,
                           buffer.config,
                           folder.iucn.buffer,
                           sampling.effort,
                           quantile.absence.certain,
                           prop.prey.certain,
                           uncertain.value,
                           backup.iucn,
                           ...) {
  cli_h2("Set gbif parameters")
  out <- new("param_gbif")
  
  #  gbif sampling activated by default
  if (missing(use.gbif)) {
    cli_alert_info("gbif sampling activated")
    use.gbif <- TRUE
  }
  if (!use.gbif) cli_alert_info("no gbif sampling")
  out@use.gbif <- use.gbif
  
  # folder.gbif
  if (missing(folder.gbif)) {
    if (use.gbif) { 
      cli_alert_danger("Please provide folder.gbif")
      stop("Missing folder.gbif")
    }
  } else {
    out@folder.gbif <- folder.gbif
  }
  
  # min.gbif
  if (missing(min.gbif)) {
    out@min.gbif <- 25
    if (use.gbif) cli_alert_info("Minimum number of gibf occurrences set to 25")
  } else {
    out@min.gbif <- min.gbif
  }
  
  # filter.atlas
  if (missing(filter.atlas)) {
    out@filter.atlas <- TRUE
    if (use.gbif) cli_alert_info("filter.atlas set to TRUE")
  } else {
    out@filter.atlas <- filter.atlas
  }
  
  # buffer.config
  if (missing(buffer.config)) {
    if (use.gbif) { 
      cli_alert_danger("Please provide argument buffer.config, a named list with the distance used to buffer IUCN range, by taxonomic group")
      stop("Missing buffer.config")
    }
  } else {
    out@buffer.config <- buffer.config
  }
  
  # sampling.effort
  if (missing(sampling.effort)) {
    if (use.gbif) { 
      cli_alert_danger("Please provide sampling.effort")
      stop("Missing sampling.effort")
    }
  } else {
    out@sampling.effort <- sampling.effort
  }
  
  # folder.iucn.buffer
  
  if (missing(folder.iucn.buffer)) {
    if (use.gbif) { 
      cli_alert_danger("Please provide folder.iucn.buffer")
      stop("Missing folder.iucn.buffer")
    }
  } else {
    out@folder.iucn.buffer <- folder.iucn.buffer
  }
  
  # quantile.absence.certain
  if (missing(quantile.absence.certain)) {
    out@quantile.absence.certain <- 0.1
    if (use.gbif) cli_alert_info("quantile.absence.certain set to 10%")
  } else {
    out@quantile.absence.certain <- quantile.absence.certain
  }
  
  # prop.prey.certain
  if (missing(prop.prey.certain)) {
    out@prop.prey.certain <- 0.8
    if (use.gbif) cli_alert_info("prop.prey.certain set to 80%")
  } else {
    out@prop.prey.certain <- prop.prey.certain
  }
  
  # uncertain.value
  if (missing(uncertain.value)) {
    out@uncertain.value <- 0
    if (use.gbif) cli_alert_info("uncertain.value set to 0")
  } else {
    out@uncertain.value <- uncertain.value
  }
  # backup.iucn
  if (missing(backup.iucn)) {
    if (use.gbif) {
      out@backup.iucn <- set_backup_iucn(...)
    } else {
      out@backup.iucn <- set_backup_iucn(do.backup = FALSE)
    }
  } else {
    out@backup.iucn <- backup.iucn
  }
  
  if (use.gbif) {
    validObject(out)
  }
  out
}


## 4.3 Methods -------------------------------------------------------------
### show.param_gbif    --------------------------------------------------
##'
##' @rdname param_gbif
##' @importMethodsFrom methods show
##' @param object an object of class \code{param_gbif}
##' @importFrom cli cli_h2 cli_h3 cli_li cli_text
##' @export
##'

setMethod('show', signature('param_gbif'),
          function(object)
          {
            if (!object@use.gbif) {
              cli_alert_warning("No gbif data used")
            } else {
              buffer.text <-
                paste0(
                  sapply(seq_along(object@buffer.config), function(i){
                    paste0(names(object@buffer.config)[i], ' (', object@buffer.config[[i]], 'km)')
                  }), 
                  collapse = ', ')
              
              cli_h2("gbif data configuration")
              cli_h3("data folder")
              cli_li("Folder with gbif data: {object@folder.gbif}")
              cli_li("Folder with IUCN buffered distribution: {object@folder.iucn.buffer}")
              cli_h3("parameters")
              cli_li("Quantile to consider absences as certain = {object@quantile.absence.certain}")
              cli_li("Proportion of prey required as certain = {object@prop.prey.certain}")
              cli_li("Values filled when prey are uncertain = {object@uncertain.value}")
              cli_li("Minimum number of occurrences required = {object@min.gbif}")
              cli_li("Filtering of atlas data = {object@filter.atlas}")
              cli_li("IUCN buffer configuration: {buffer.text}")
              show(object@backup.iucn)
              show(object@sampling.effort)
            }
            invisible(NULL)
          })
## --------------------------------------------------------------------------- #
# 5. param_subsampling         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name param_subsampling
##' @aliases param_subsampling-class
##' @author Remi Patin
##'
##' @title Parameters for subsampling dataset
##'
##' @description Class contained within a \code{\link{param_trophic}} object. A
##'   \code{param_subsampling} object contains the set of parameters used to 
##'   extract subsample occurrences in the final dataset. It contains:
##'  \itemize{
##'   \item the method used (only random for now)
##'   \item the maximum number of presences
##'   \item the maximum number of absences inside IUCN range
##'   \item the minimum number of absences outside IUCN range (if applicable)
##'   \item the maximum number of absences outside IUCN range (if applicable)
##'   \item the target proportion of absences outside IUCN range (relative to 
##'   total data within IUCN range)
##'   }
##'
##' @slot method a \code{logical}, whether gbif was used or not
##' @slot max.presences a \code{integer} maximum number of presences
##' @slot max.absences.inside a \code{integer} maximum number of absences inside
##'   IUCN range
##' @slot min.absences.outside a \code{integer} minimum number of absences
##'   outside IUCN range
##' @slot max.absences.outside a \code{integer} maximum number of absences
##'   outside IUCN range
##' @slot prop.outside a \code{numeric}, target proportion of
##'   absences outside IUCN range (relative to total data within IUCN range)


## 5.1 Class Definition ----------------------------------------------------------------------------

setClass("param_subsampling",
         representation(
           method = "character",
           max.presences = "numeric",
           max.absences.inside = "numeric",
           min.absences.outside = "numeric",
           max.absences.outside = "numeric",
           prop.outside = "numeric"),
         validity = function(object){
           .fun_testIfIn(object@method, c("none", "random"))
           if (.fun_testIfPosNum(object@max.presences)) {
             if (is.finite(object@max.presences)) {
               .fun_testIfPosInt(object@max.presences)
             }
           }
           
           if (.fun_testIfPosNum(object@max.absences.inside)) {
             if (is.finite(object@max.absences.inside)) {
               .fun_testIfPosInt(object@max.absences.inside)
             }
           }
           
           .fun_testIfPosInt(object@min.absences.outside)
           if (.fun_testIfPosNum(object@max.absences.outside)) {
             if (is.finite(object@max.absences.outside)) {
               .fun_testIfPosInt(object@max.absences.outside)
             }
           }
           .fun_testIfPosNum(object@prop.outside)
           stopifnot(is.finite(object@prop.outside))
           TRUE
         })



## 5.2 Constructor ------------------------------------------------------------
##' 
##' @rdname param_subsampling
##' @export
##' 
##' @importFrom methods validObject
##' @param method a \code{logical}, whether gbif was used or not
##' @param max.presences a \code{integer} maximum number of presences
##' @param max.absences.inside a \code{integer} maximum number of absences inside
##'   IUCN range
##' @param min.absences.outside a \code{integer} minimum number of absences
##'   outside IUCN range
##' @param max.absences.outside a \code{integer} maximum number of absences
##'   outside IUCN range
##' @param prop.outside a \code{numeric}, target proportion of
##'   absences outside IUCN range (relative to total data within IUCN range)

set_param_subsampling <- function(
    method,
    max.presences,
    max.absences.inside,
    min.absences.outside,
    max.absences.outside,
    prop.outside) {
  cli_h2("Set subsampling parameters")
  out <- new("param_subsampling")
  
  #  random subsampling activated by default
  if (missing(method)) {
    cli_alert_info("random subsampling activated")
    method <- "random"
  }
  out@method <- method
  if (method != "none") do.subsampling <- TRUE
  
  # max.presences
  if (missing(max.presences)) {
    out@max.presences <- 1e5
    if (do.subsampling) cli_alert_info("Maximum number of presences set to 100 000")
  } else {
    out@max.presences <- max.presences
  }
  
  # max.absences.inside
  if (missing(max.absences.inside)) {
    out@max.absences.inside <- 1e5
    if (do.subsampling) cli_alert_info("Maximum number of absences inside IUCN range\\
                                       set to 100 000")
  } else {
    out@max.absences.inside <- max.absences.inside
  }
  
  
  # min.absences.outside
  
  if (missing(min.absences.outside)) {
    out@min.absences.outside <- 3000
    if (do.subsampling) cli_alert_info("Minimum number of absences outside IUCN range\\
                                       set to 3000")
  } else {
    out@min.absences.outside <- min.absences.outside
  }
  
  # max.absences.outside
  if (missing(max.absences.outside)) {
    out@max.absences.outside <- 20000
    if (do.subsampling) cli_alert_info("Maximum number of absences outside IUCN range\\
                                       set to 20000")
  } else {
    out@max.absences.outside <- max.absences.outside
  }
  
  # prop.outside
  if (missing(prop.outside)) {
    out@prop.outside <- 3
    if (do.subsampling) cli_alert_info("prop.outside set to 3")
  } else {
    out@prop.outside <- prop.outside
  }
  validObject(out)
  out
}

## 5.3 Methods -------------------------------------------------------------
### show.param_subsampling -------------------------------------------------
##'
##' @rdname param_subsampling
##' @importMethodsFrom methods show
##' @param object an object of class \code{param_subsampling}
##' @importFrom cli cli_h2 cli_h3 cli_li cli_text
##' @export
##'

setMethod('show', signature('param_subsampling'),
          function(object)
          {
            if (object@method == "none") {
              cli_alert_warning("No subsampling")
            } else {
              cli_h2("Parameters for subsampling")
              if (object@max.presences < Inf) {
                cli_li("Subsampling presences to have a maximum of {object@max.presences}")
              }
              
              if (object@max.absences.inside < Inf) {
                cli_li("Subsampling absences inside IUCN range to have a maximum of {object@max.absences.inside}")
              }
              
              cli_li(
                "Subsample absences outside IUCN range to have a minimum of \\
{object@min.absences.outside}, \\
a target of {object@prop.outside} times \\
the data inside IUCN range and a maximum of \\
{object@max.absences.outside} absences"
              )             
            }
            invisible(NULL)
          })
## --------------------------------------------------------------------------- #
# 6. param_trophic         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name param_trophic
##' @aliases param_trophic-class
##' @author Remi Patin
##'
##' @title Parameters of a trophic_dataset
##'
##' @description Class contained within \code{\link{trophic_dataset}}. A
##'   \code{param_trophic} object contains the set of parameters used to 
##'   generate the dataset:
##'   \itemize{
##'   \item the final and raw checklist of species 
##'   \item the final and raw metaweb 
##'   \item the set of parameters used for gbif extraction
##'   \item the folder with IUCN ranges
##'   \item the set of parameters used for subsampling
##'   }
##'
##' @slot checklist a \code{data.frame} with information on all species 
##'   that remain after filtering of species.
##' @slot metaweb a \code{data.frame} with all species interactions 
##' that remain after filtering of species and prey.
##' @slot trophic.groups a \code{data.frame}
##' @slot checklist.raw a \code{data.frame} with original information on all
##'   species
##' @slot metaweb.raw a \code{data.frame} with all original species interactions
##' @slot trophic.groups.raw a \code{data.frame}
##' @slot param.gbif a \code{\link{param_gbif}} object with all parameters used
##'   to extract gbif data
##' @slot param.subsampling a \code{\link{param_subsampling}} object with all
##'   parameters used for subsampling
##' @slot folder.iucn  a \code{character}, folder in which IUCN range
##' are stored as raster files.


## 6.1 Class Definition ----------------------------------------------------------------------------

setClass("param_trophic",
         representation(checklist = "data.frame",
                        metaweb = "data.frame",
                        trophic.groups = "data.frame",
                        checklist.raw = "data.frame",
                        metaweb.raw = "data.frame",
                        trophic.groups.raw = "data.frame",
                        param.gbif = "param_gbif",
                        param.subsampling = "param_subsampling",
                        folder.iucn = "character"),
         validity = function(object){
           .check_checklist(object@checklist)
           .check_checklist(object@checklist.raw)
           .check_metaweb(object@metaweb)
           .check_metaweb(object@metaweb.raw)
           .fun_testIfDirExists(folder.iucn)
           TRUE
         })

## 6.2 Methods -------------------------------------------------------------
### show.param_trophic    --------------------------------------------------
##'
##' @rdname param_trophic
##' @importMethodsFrom methods show
##' @param object an object of class \code{param_trophic}
##' @importFrom cli cli_h1 cli_h2 cli_h3 cli_li cli_text
##' @export
##'

setMethod('show', signature('param_trophic'),
          function(object)
          {
            cli_h1("Parameters for extracting trophic dataset")
            intro.text <- paste0("Modeling ", nrow(object@checklist), " species with ",
                                 nrow(object@metaweb), " predator-prey interactions,")
            if(object@param.gbif@use.gbif) {
              if(object@param.gbif@backup.iucn@do.backup){
                cli_alert_info("{intro.text} using gbif data if possible, IUCN if not.")
              }  else {
                cli_alert_info("{intro.text} using only gbif data")
              }
            } else {
              cli_alert_info("{intro.text} using only IUCN data")
            }
            cli_li("Folder with IUCN range data: {object@folder.iucn}")
            show(object@param.gbif)
            show(object@param.subsampling)
            invisible(NULL)
          })

## --------------------------------------------------------------------------- #
# 7. trophic_species         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name trophic_species
##' @aliases trophic_species-class
##' @author Remi Patin
##'
##' @title Status of each species within a \code{\link{trophic_dataset}}
##'
##' @description Class contained within \code{\link{trophic_dataset}}. A
##'   \code{trophic_species} object contains a set of information on the different
##'   species that went through the workflow:
##'   \itemize{
##'   \item the species kept in the final dataset
##'   \item the species filtered out through the workflow
##'   \item the prey filtered out for each predator
##'   \item the method used for each species kept (IUCN or gbif)
##'   \item whether data was freshly extracted or reused from a previous extraction
##'   }
##'
##' @slot kept a named \code{character} vector with all species kept and the method used
##' @slot filtered a named \code{character} vector with all species removed 
##' as well as the motivation of removal
##' @slot kept.prey a named \code{list} with the prey kept for each species
##' @slot filtered.prey a named \code{list} vector with the prey removed for 
##' each species as well as the motivation of removal
##' @slot fresh.occurrence a named \code{logical} vector. For each species
##'   whether occurrence extraction was freshly done or not
##' @slot fresh.trophic a named \code{logical} vector. For each species whether
##'   trophic extraction was freshly done or not


## 7.1 Class Definition ----------------------------------------------------------------------------

setClass("trophic_species",
         representation(kept = "character",
                        filtered = "character",
                        kept.prey = "list",
                        filtered.prey = "list",
                        species.method = "factor",
                        fresh.occurrence = "character",
                        fresh.trophic = "character"),
         validity = function(object){
           .fun_testIfInherits(names(object@kept), "character")
           .fun_testIfInherits(names(object@filtered), "character")
           .fun_testIfInherits(names(object@kept.prey), "character")
           .fun_testIfInherits(unlist(object@kept.prey), "character")
           .fun_testIfInherits(names(object@filtered.prey), "character")
           .fun_testIfInherits(unlist(object@filtered.prey), "character")
           .fun_testIfInherits(names(object@fresh.occurrence), "character")
           .fun_testIfInherits(names(object@fresh.trophic), "character")
           TRUE
         })


## 7.2 Methods -------------------------------------------------------------
### show.trophic_species    --------------------------------------------------
##'
##' @rdname trophic_species
##' @importMethodsFrom methods show
##' @param object an object of class \code{trophic_species}
##' @importFrom cli cli_h1 cli_h2 cli_h3 cli_li cli_text
##' @export
##'

setMethod('show', signature('trophic_species'),
          function(object)
          {
            species.method <- object@species.method
            cli_h3("List of species used")
            cli_li("{length(object@kept)} species kept, {length(object@filtered)} filtered")
            cli_li("{length(unlist(object@kept.prey))} predator-prey interactions kept, {length(unlist(object@filtered.prey))} filtered")
            cli_h3("Method used for species")
            cli_li("{sapply(1:3, 
       function(i){glue('{levels(species.method)[i]} ({table(species.method)[i]})')})
}")
            invisible(NULL)
          })


## --------------------------------------------------------------------------- #
# 8. trophic_dataset         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name trophic_dataset
##' @aliases trophic_dataset-class
##' @author Remi Patin
##'
##' @title Dataset for running trophic SDM
##'
##' @description Class returned by \code{\link{prepare_dataset}} that prepare 
##' a dataset for a trophic SDM, based on a gbif, IUCN or mixed workflow.
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @inheritParams rasterize_iucn
##' @param folder.iucn a \code{character}, folder in which IUCN buffered range
##' are stored as raster files.
##' @slot summary a \code{\link{trophic_summary}} with summary information on the workflow
##' @slot summary.raw a \code{\link{trophic_summary}} with raw summary
##'   information on the workflow (before filtering and subsampling)
##' @slot files a \code{\link{trophic_files}} with links to data files
##' @slot param a \code{\link{param_trophic}} with the parameter used in the 
##' workflow
##' @slot species a \code{\link{trophic_species}} with information on all species 
##'   that remains after filtering of species.
##' @slot project.name a \code{character} indicating the folder in which
##'   logfiles and data may be written
##' @slot data.mask a \code{SpatRaster} with the extent and the grid used 
##' in the project
##' 
##' @examples
##'
##' showClass("trophic_dataset")
NULL

##' @name trophic_dataset-class
##' @rdname trophic_dataset
##' @export
##' @importFrom terra rast

## 8.1 Class Definition ----------------------------------------------------------------------------


setClass("trophic_dataset",
         representation(summary = "trophic_summary",
                        summary.raw = "trophic_summary",
                        files = "trophic_files",
                        param = "param_trophic",
                        species = "trophic_species",
                        project.name = "character",
                        data.mask = "PackedSpatRaster"),
         validity = function(object){
           TRUE
         })


## 8.2 Methods -------------------------------------------------------------
### show.trophic_dataset    --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @importMethodsFrom methods show
##' @param object an object of class \code{trophic_dataset}
##' @importFrom cli cli_h2 cli_h3 cli_li
##' @export
##'

setMethod('show', signature('trophic_dataset'),
          function(object)
          {
            cli_h1("Trophic dataset project {object@project.name}")
            show(object@species)
            show(object@param)
            invisible(NULL)
          })




### subsample  ------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'


setGeneric("subsample", def = function(x, ...) {
  standardGeneric("subsample") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_alert_success
setMethod('subsample', signature(x = 'trophic_dataset'),
          function(x,
                   param.subsampling) {
            ### Argument check and setup --------------------------------------
            args <- .subsample.check.args(x = x, 
                                          param.subsampling = param.subsampling)
            for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
            
            
            # setup dir
            project.name <- x@project.name
            folder.names <- set_folder(project.name)
            for (argi in names(folder.names)) {  assign(x = argi, value = folder.names[[argi]]) }
            
            # code-name converter
            listcode.full <- x@param@checklist.raw$Code
            names(listcode.full) <- x@param@checklist.raw$SpeciesName
            listname.full <- x@param@checklist.raw$SpeciesName
            names(listname.full) <- x@param@checklist.raw$Code
            
            listcode <- x@param@checklist$Code
            names(listcode) <- x@param@checklist$SpeciesName
            
            ### Subsampling ----------------------------------------------------
            if (param.subsampling@method != "none") {
              
              cli_h2("Subsampling")
              
              if (param.subsampling@max.presences < Inf) {
                cli_alert_info("Subsampling presences to have a maximum of {param.subsampling@max.presences}")
              }
              
              if (param.subsampling@max.absences.inside < Inf) {
                cli_alert_info("Subsampling absences inside IUCN range to have a maximum of {param.subsampling@max.absences.inside}")
              }
              
              cli_alert_info(
                "Subsample absences outside IUCN range to have a minimum of \\
{param.subsampling@min.absences.outside}, \\
a target of {param.subsampling@prop.outside} times \\
the data inside IUCN range and a maximum of \\
{param.subsampling@max.absences.outside} absences"
              )
              
              
              
              summary.trophic <- x@summary.raw@trophic  %>% 
                mutate(
                  aim_presence = pmin(presence, param.subsampling@max.presences),
                  aim_absence_inside = pmin(presence, param.subsampling@max.absences.inside),
                  tot_inside = aim_presence + aim_absence_inside,
                  aim_outside =
                    pmin(
                      pmax(param.subsampling@min.absences.outside,
                           tot_inside*param.subsampling@prop.outside),
                      param.subsampling@max.absences.outside
                    ),
                  filter_presence = presence > aim_presence,
                  filter_absence_inside = absence_inside > aim_absence_inside,
                  filter_absence_outside = absence_outside > aim_outside
                )  %>% 
                filter(filter_presence | filter_absence_inside | filter_absence_outside)
              
              if (nrow(summary.trophic) == 0) {
                cli_alert_success("No species require subsampling")
              } else {
                this.species <- "Cervus elaphus"
                for (this.species in summary.trophic$SpeciesName) {
                  cli_progress_step(this.species)
                  this.code <- listcode[this.species]
                  # browser()
                  this.raw <- load_data(x,
                                        SpeciesName = this.species,
                                        type = "trophic.raw")
                  this.raw$status <- "certain"
                  this.raw <- subsample_dataset(this.raw, param.subsampling)
                  #### write output and calculate summary ------------------------------------
                  
                  # write trophic.raw with subsampling info
                  fwrite(this.raw, 
                         file = x@files@trophic.raw[this.species],
                         showProgress = FALSE)

                  # generate trophic files (subsampling + remove prey)
                  # remove subsampling
                  this.trophic <- filter(this.raw, subsample)
                  
                  # write trophic file
                  file.trophic <- paste0(project.name, "/trophic_dataset/", this.code,".csv.gz")
                  fwrite(this.trophic, 
                         file = file.trophic, 
                         showProgress = FALSE)
                  x@files@trophic[this.species] <- file.trophic
                  x@summary@trophic <- 
                    filter( x@summary@trophic, SpeciesName != this.species) %>% 
                    rbind(get_trophic_summary(this.trophic))
                }
                cli_progress_done()
              }
              cli_alert_success("All species subsampled")
            }
            saveRDS(x, file = paste0(x@project.name,"/trophic_dataset.rds"))
            x
          })

#### Argument Check -----------------------------------

.subsample.check.args <- function(x, param.subsampling){
  
  if (missing(param.subsampling)) {
    param.subsampling <- x@param@param.subsampling
  } else {
    .fun_testIfInherits(param.subsampling, "param_subsampling")
    x@param@param.subsampling <- param.subsampling
  }
  return(list(
    x = x,
    param.subsampling = param.subsampling
  ))
}


### reset    --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'


setGeneric("reset", def = function(x, ...) {
  standardGeneric("reset") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_progress_step cli_progress_done cli_progress_update cli_progress_bar
##' 
setMethod('reset', signature(x = 'trophic_dataset'),
          function(x, type = "full") {
            
            .fun_testIfIn(type, c("full","species","prey","subsampling"))
            
            # code-name converter
            listcode.full <- x@param.raw$checklist$Code
            names(listcode.full) <- x@param.raw$checklist$SpeciesName
            listname.full <- x@param.raw$checklist$SpeciesName
            names(listname.full) <- x@param.raw$checklist$Code
            
            folder.names <- set_folder(x@project.name)
            for (argi in names(folder.names)) { 
              assign(x = argi, value = folder.names[[argi]]) 
            }
            
            if (type == "full") {
              cli_progress_step("Full reset of trophic dataset")
              x@summary.occurrence <- x@param.raw$summary.occurrence.raw
              x@summary.predator <- x@param.raw$summary.predator.raw
              x@summary.prey <- x@param.raw$summary.prey.raw
              x@metaweb.filtered <- x@param.raw$metaweb
              x@checklist.filtered <- x@param.raw$checklist
              x@kept.species <- x@param.raw$kept.species.raw
              x@filtered.species <- x@param.raw$filtered.species.raw
              x@kept.prey <- x@param.raw$kept.prey.raw
              x@filtered.prey <- x@param.raw$filtered.prey.raw
              # reset files
              file.remove(list.files(dataset.dir, full.names = TRUE))
              x@file.trophic.link <- x@param.raw$file.trophic.raw.link
              x@file.occurrence.link <- x@param.raw$file.occurrence.raw.link
              x@metaweb.filtered <- 
                x@metaweb.filtered %>% 
                filter(Pred_Code_new %in% x@summary.prey$Code &
                         Prey_Code_new %in% x@summary.prey$Prey_Code)
              
              if (x@param$param.filter$subsample.method != "none") { 
                species.list <- names(x@file.trophic.link)
                cli_progress_bar("Resetting trophic data", total = length(species.list))
                for (this.species in species.list) {
                  cli_progress_update()
                  this.file <- x@file.trophic.link[this.species]
                  this.df <- fread(this.file, showProgress = FALSE)
                  this.df$subsample <- TRUE
                  fwrite(this.df, this.file, showProgress = FALSE)
                }
                cli_progress_done()
              }
              
              x@checklist.filtered <- 
                x@checklist.filtered %>% 
                filter(Code %in% x@summary.occurrence$Code)
              
              x@param$param.filter <- 
                list(min.presence = 0, 
                     min.absence = 0, 
                     subsample.method = "none", 
                     subsample.regions = NA, 
                     subsample.min.absence.outside = NA, 
                     subsample.max.absence.outside = NA, 
                     subsample.prop.outside = NA, 
                     min.prevalence.prey = 0,
                     min.absence.prey = 0)
              cli_progress_done()
            } else {
              cli_alert_danger("reset can only be used with type = 'full' for now")
              stop("wrong type option")
            }
            # else if (type == "species") {
            #   species.reset <- names(which(x@filtered.species == "Not enough data after rasterization and prey filtering"))
            #   x@param$param.filter$min.presence <- 0 
            #   x@param$param.filter$min.absence <- 0 
            #   param.filter <- x@param$param.filter
            #   if (length(species.reset) > 0) {
            #     x@filtered.species <- x@param.raw$filtered.species.raw
            #     
            #     # 1. reset occurrence
            #     x@file.occurrence.link <- 
            #       x@param.raw$file.occurrence.raw.link
            #     # 2. reset trophic.raw
            #     for (this.species in species.reset) {
            #       x@file.trophic.link[this.species] <-
            #         x@param.raw$file.trophic.raw.link[this.species]
            #     }
            #     # 3. add to predators if need be
            #     prey.to.add <- lapply(x@filtered.prey, function(y){
            #       names(y)[which(names(y) %in% species.reset)]
            #     })
            #     this.pred <- "Natrix natrix"
            #     for (this.pred in names(prey.to.add)) {
            #       this.prey.add <- prey.to.add[[this.pred]]
            #       # is a predator that lost prey
            #       if (length(this.prey.add) > 0) {
            #         cli_progress_step("Updating predator {this.pred}")
            #         names(this.prey.add) <- listcode.full[this.prey.add]
            #         this.prey.select <- 
            #           x@param.raw$summary.prey.raw %>%
            #           filter(
            #                SpeciesName == this.pred,
            #                Prey_Code %in% names(this.prey.add),
            #                prevalence_pred1 >= param.filter$min.prevalence.prey,
            #                absence >= param.filter$min.absence.prey
            #                )
            #         
            #         if (nrow(this.prey.select) > 0) {
            #           this.prey.add <- this.prey.add[which(names(this.prey.add) %in%
            #                                                  this.prey.select$Prey_Code)]
            #           x@filtered.prey[[this.pred]] <- x@filtered.prey[[this.pred]][
            #             ! names(x@filtered.prey[[this.pred]]) %in% this.prey.add
            #           ]
            #           x@kept.prey[[this.pred]] <- c(x@kept.prey[[this.pred]],
            #                                         this.prey.add)
            #           this.df <-
            #             load_data(x, SpeciesName = this.pred, type = "trophic.raw") %>%
            #             filter(subsample)
            #           for (this.prey in listcode.full[names(x@filtered.prey[[this.pred]])]) {
            #             this.df[,this.prey] <- NULL
            #           }
            #           this.file <- paste0(x@project.name,"/trophic_dataset/", 
            #                               listcode.full[this.pred],".csv.gz")
            #           fwrite(this.df, this.file)
            #           x@file.trophic.link[this.pred] <- this.file
            #           cli_progress_done()
            #         }
            #       }
            #     }
            #     # 4 - subsampling new species
            #     if (param.filter$subsample.method != "none") {
            #       x <- subsample(x,
            #                      subsample.method = param.filter$subsample.method,
            #                      subsample.min.absence.outside = param.filter$subsample.min.absence.outside,
            #                      subsample.max.absence.outside = param.filter$subsample.max.absence.outside,
            #                      subsample.prop.outside = param.filter$subsample.prop.outside)
            #     }
            #   }
            # } else if (type == "prey") {
            #   browser()
            # }
            
            saveRDS(x, file = paste0(x@project.name,"/trophic_dataset.rds"))
            return(x)
          })



### summary_trophic    --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'


setGeneric("summary_trophic", def = function(x, ...) {
  standardGeneric("summary_trophic") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_alert_success
##' @param info the type of info return
setMethod('summary_trophic', signature(x = 'trophic_dataset'),
          function(x,
                   info = "trophic_dataset") {
            
            possible_info <- c("summary.occurrence",
                               "summary.uncertain",
                               "summary.trophic",
                               "summary.trophic.raw",
                               "summary.prey",
                               "summary.protected",
                               "metaweb",
                               "checklist",
                               "kept.species",
                               "kept.prey",
                               "filtered.species",
                               "filtered.prey")
            .fun_testIfIn(info, possible_info)
            
            if (info == "summary.occurrence") {
              return(x@summary@occurrence)
            } else if (info == "summary.uncertain") {
              output <- x@summary@occurrence %>% 
                mutate(tot.iucn =
                         absence.inside.certain + 
                         absence.inside.uncertain +
                         presence,
                       prop.presence = presence/tot.iucn,
                       prop.certain = absence.inside.certain/tot.iucn,
                       prop.uncertain = absence.inside.uncertain/tot.iucn) %>% 
                left_join(x@param@checklist, by = c("SpeciesName","Code"))
              return(output)
            } else if (info == "summary.trophic") {
              return(x@summary@trophic)
            } else if (info == "summary.trophic.raw") {
              return(x@summary.raw@trophic)
            } else if (info == "summary.prey") {
              return(x@summary@prey)
            } else if (info == "summary.filter") {
              return(x@summary@filtered)
            } else if (info == "summary.protected") {
              return(x@summary@protected)
            } else if (info == "metaweb") {
              return(x@param@metaweb)
            } else if (info == "checklist") {
              return(x@param@checklist)
            } else if (info == "filtered.species") {
              tmp <- x@species@filtered
              output <- data.frame(SpeciesName = names(tmp),
                                   reason = unname(tmp))
              return(output)
              
            } else if (info == "filtered.prey") {
              tmp.prey <- x@species@filtered.prey
              output <- foreach(this.species = names(tmp.prey), .combine = 'rbind') %do% {
                tmp <- tmp.prey[[this.species]]
                if (length(tmp) == 0) {
                  return(NULL)
                }
                data.frame(Pred_Name = this.species,
                           Prey_Name = names(tmp),
                           reason = unname(tmp))
              }            
              return(output)
            } else if (info == "kept.species") {
              return(x@species@kept)
            } else if (info == "kept.prey") {
              return(x@species@kept.prey)
            }
          })

### plot_uncertain    --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'
##' @importFrom ggplot2 scale_x_continuous


setGeneric("plot_uncertain", def = function(x, ...) {
  standardGeneric("plot_uncertain") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_alert_success
##' @importFrom tidyterra geom_spatraster
##' @importFrom ggplot2 geom_tile ggplot
##' @importFrom cowplot save_plot

setMethod('plot_uncertain', signature(x = 'trophic_dataset'),
          function(x, nb.cpu = 1){
            checklist <- summary_trophic(x, info = "checklist")
            .fun_testIfPosInt(nb.cpu)
            has.cluster <- .register_cluster(nb.cpu)
            listcode <- checklist$Code
            names(listcode) <- checklist$SpeciesName
            listClass <- checklist$Class
            names(listClass) <- checklist$SpeciesName
            listOrder <- checklist$Order
            names(listOrder) <- checklist$SpeciesName
            plot.folder <- paste0(x@project.name, "/plot.uncertain/")
            if (!dir.exists(plot.folder)) dir.create(plot.folder, showWarnings = FALSE)
            summary.uncertain <- summary_trophic(x, info = "summary.uncertain")
            
            data.mask <- unwrap(x@data.mask)
            this.species <- "Capra ibex"  
            foreach(this.species = names(listcode)) %dopar% {
              cli_progress_step(this.species)
              this.Order <- listOrder[this.species]
              this.Class <- listClass[this.species]
              this.summary <- filter(summary.uncertain, SpeciesName == this.species)
              this.file <- paste0(plot.folder, this.Class, "_", this.Order, "_", this.species, ".png")
              this.title <- paste0(this.species, 
                                   "\n presence: ", this.summary$presence, " (",
                                   round(this.summary$prop.presence*100), "%)",
                                   "\n uncertain cells: ",
                                   this.summary$absence.inside.uncertain, " (",
                                   round(this.summary$prop.uncertain*100), "%)")
              this.df <- load_data(x, SpeciesName = this.species, type = "occurrence") %>% 
                filter(inside_iucn) %>% 
                mutate(datatype = ifelse(presence == 1, "presence", paste0(status, " absence")))
              g <- ggplot() +
                geom_spatraster(data = mutate(data.mask, layer = as.character(layer))) +
                geom_tile(data = this.df, aes(x = x, y = y, fill = datatype)) +
                scale_fill_manual(
                  "IUCN distribution",
                  values = c("grey40", "#d95f02", "#7570b3", "#1b9e77"),
                  breaks = c("1","presence", "certain absence", "uncertain absence"),
                  label = c("Outside IUCN", "Presence", "Absence (certain)", "Absence (uncertain)"),
                  na.value = "grey") +
                scale_x_continuous(limits = range(this.df$x)) +
                scale_y_continuous(limits = range(this.df$y)) +
                ggtitle(this.title)
              save_plot(this.file, g, base_width = 20/cm(1), base_height = 18/cm(1))
              cli_progress_done()
              NULL
            }
            invisible(NULL)
          })


### plot_trophic    --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'


setGeneric("plot_trophic", def = function(x, ...) {
  standardGeneric("plot_trophic") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_alert_success
##' @importFrom tidyterra geom_spatraster
##' @importFrom ggplot2 geom_tile ggplot
##' @importFrom cowplot save_plot
##' @importFrom dplyr anti_join

setMethod('plot_trophic', signature(x = 'trophic_dataset'),
          function(x, nb.cpu = 1){
            checklist <- summary_trophic(x, info = "checklist")
            .fun_testIfPosInt(nb.cpu)
            has.cluster <- .register_cluster(nb.cpu)
            listcode <- checklist$Code
            names(listcode) <- checklist$SpeciesName
            listClass <- checklist$Class
            names(listClass) <- checklist$SpeciesName
            listOrder <- checklist$Order
            names(listOrder) <- checklist$SpeciesName
            plot.folder <- paste0(x@project.name, "/plot.trophic/")
            if (!dir.exists(plot.folder)) dir.create(plot.folder, showWarnings = FALSE)
            data.mask <- unwrap(x@data.mask)
            
            summary.uncertain <- summary_trophic(x, info = "summary.uncertain")
            this.species <- "Canis lupus" 
            foreach(this.species = names(listcode)) %dopar% {
              cli_progress_step(this.species)
              # browser()
              this.prey.filtered <- length(x@species@filtered.prey[[this.species]])
              this.prey.kept <- length(x@species@kept.prey[[this.species]])
              this.prey <- this.prey.filtered + this.prey.kept
              if (this.prey > 0) {
                
                
                this.Order <- listOrder[this.species]
                this.Class <- listClass[this.species]
                this.summary <- filter(summary.uncertain, SpeciesName == this.species)
                this.file <- paste0(plot.folder, this.Class, "_", this.Order, "_", this.species, ".png")
                
                this.occurrence <- load_data(x, 
                                             SpeciesName = this.species,
                                             type = "occurrence") 
                this.df <- 
                  this.occurrence %>% 
                  filter(inside_iucn) %>% 
                  mutate(datatype = ifelse(presence == 1, "presence", paste0(status, " absence")))
                this.trophic <- load_data(x, 
                                          SpeciesName = this.species,
                                          type = "trophic") %>% 
                  select(cell, x, y)
                
                this.trophic.filtered <- 
                  this.occurrence %>% 
                  filter(status != "uncertain") %>% 
                  anti_join(this.trophic, by = c("cell", "x", "y")) %>% 
                  mutate(datatype = ifelse(presence == 1, "filtered presence", 
                                           ifelse(inside_iucn,
                                                  "filtered absence within IUCN",
                                                  "filtered absence outside IUCN")))
                # nrow(this.trophic.filtered)
                # nrow(this.occurrence)
                filtered.presence <- length(which(this.trophic.filtered$datatype == 
                                                    "filtered presence"))
                filtered.absence.outside <- length(which(this.trophic.filtered$datatype == 
                                                           "filtered absence outside IUCN"))
                filtered.absence.within <- length(which(this.trophic.filtered$datatype == 
                                                          "filtered absence within IUCN"))
                this.title <- paste0(this.species, 
                                     "\n filtered cells: ", nrow(this.trophic.filtered), " (",
                                     round(nrow(this.trophic.filtered)/
                                             nrow(filter(this.occurrence, status != "uncertain"))*100), "%)",
                                     " ; filtered presence: ", 
                                     filtered.presence, "/", this.summary$presence, 
                                     " ; filtered absence inside IUCN: ", 
                                     filtered.absence.within, "/", this.summary$absence.inside.certain, 
                                     " ; filtered absence outside IUCN: ", 
                                     filtered.absence.outside, "/", this.summary$absence.outside, 
                                     "\n filtered prey: ",
                                     this.prey.filtered, "/", this.prey)
                
                g <- ggplot() +
                  geom_spatraster(data = mutate(data.mask, layer = as.character(layer))) +
                  geom_tile(data = this.df, aes(x = x, y = y, fill = datatype)) +
                  geom_tile(data = this.trophic.filtered, aes(x = x, y = y, fill = datatype)) +
                  scale_fill_manual(
                    "IUCN distribution",
                    values = c("grey40", "#d95f02", "#7570b3", "grey70",
                               "#895f02", "#353073", "grey20"),
                    breaks = c("1","presence", "certain absence", "uncertain absence",
                               "filtered presence", "filtered absence within IUCN",
                               "filtered absence outside IUCN"),
                    label = c("Outside IUCN", "Presence", "Absence (certain)", "Absence (uncertain)",
                              "Filtered presence", "Filtered absence (IUCN)", 
                              "Filtered absence (outside)"),
                    na.value = "grey") +
                  ggtitle(this.title)
                save_plot(this.file, g, base_width = 40/cm(1), base_height = 35/cm(1))
              }
              cli_progress_done()
              NULL
            }
            invisible(NULL)
          }
)



### plot_dataset   --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'
##' @importFrom ggplot2 guide_legend


setGeneric("plot_dataset", def = function(x, ...) {
  standardGeneric("plot_dataset") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_alert_success
##' @importFrom tidyterra geom_spatraster
##' @importFrom ggplot2 geom_tile ggplot scale_color_manual guides
##' @importFrom cowplot save_plot
##' @importFrom dplyr anti_join

setMethod('plot_dataset', signature(x = 'trophic_dataset'),
          function(x, type, nb.cpu = 1){
            checklist <- summary_trophic(x, info = "checklist")
            .fun_testIfPosInt(nb.cpu)
            .fun_testIfIn(type, c("trophic", "trophic.raw"))
            has.cluster <- .register_cluster(nb.cpu)
            listcode <- checklist$Code
            names(listcode) <- checklist$SpeciesName
            listClass <- checklist$Class
            names(listClass) <- checklist$SpeciesName
            listOrder <- checklist$Order
            names(listOrder) <- checklist$SpeciesName
            plot.folder <- paste0(x@project.name, "/plot.dataset/",type,"/")
            if (!dir.exists(plot.folder)) dir.create(plot.folder, recursive = TRUE, showWarnings = FALSE)
            data.mask <- unwrap(x@data.mask)
            
            if (type == "trophic") {
              summary.trophic <- summary_trophic(x, info = "summary.trophic")  
            } else {
              summary.trophic <- summary_trophic(x, info = "summary.trophic.raw")  
            }
            this.species <- "Ardea cinerea" 
            foreach(this.species = names(listcode)) %dopar% {
              cli_progress_step(this.species)
              this.index <- which(summary.trophic$SpeciesName == this.species)
              this.prey <- summary.trophic$nprey[this.index]
              this.pres <- summary.trophic$presence[this.index]
              this.prev <- round(summary.trophic$prevalence[this.index]*100, digits = 2)
              this.abs.outside <- summary.trophic$absence_outside[this.index]
              this.abs.inside <- summary.trophic$absence_inside[this.index]
              this.Order <- listOrder[this.species]
              this.Class <- listClass[this.species]
              this.summary <- filter(summary.trophic, SpeciesName == this.species)
              this.file <- paste0(plot.folder, this.Class, "_", this.Order, "_",
                                  this.species, ".png")
              
              this.df <- 
                load_data(x, 
                          SpeciesName = this.species,
                          type = type) %>% 
                mutate(presence_plot = ifelse(presence == 1, "Presence", 
                                              ifelse(inside_iucn,
                                                     "Absence inside IUCN",
                                                     "Absence outside IUCN")))
              
              this.title <- paste0(this.species,
                                   " (nprey = ", this.prey,")",
                                   "\npresence = ", this.pres, " (", this.prev,"%)",
                                   " ; absence = ", this.abs.inside, " (inside)",
                                   " and ", this.abs.outside, " (outside)")
              
              g <- ggplot() +
                geom_spatraster(data = mutate(data.mask, layer = as.character(layer))) +
                geom_point(data = this.df, aes(x = x, y = y, color = presence_plot), size = 0.1) +
                scale_color_manual(
                  NULL,
                  values = c("#d95f02", "#7570b3", "#1b9e77"),
                  breaks = c("Presence", "Absence outside IUCN", "Absence inside IUCN"),
                  na.value = "grey") +
                scale_fill_manual(NULL,
                                  na.value = "lightblue",
                                  values = "grey",
                                  breaks = c(1),
                                  labels = c("background"))+
                guides(color = guide_legend(override.aes = list(size = 2)))+
                ggtitle(this.title)
              save_plot(this.file, g, base_width = 40/cm(1), base_height = 35/cm(1))
              cli_progress_done()
              NULL
            }
            
            
            invisible(NULL)
          }
)



### rasterize_uncertain  --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'


setGeneric("rasterize_uncertain", def = function(x, ...) {
  standardGeneric("rasterize_uncertain") 
})

##' @rdname trophic_dataset
##' @param raster.by a \code{character}, describing which taxonomic level
##' should be used to group the species (\code{Class}, \code{Order} or
##'  \code{Family})
##' @export

setMethod('rasterize_uncertain', signature(x = 'trophic_dataset'),
          function(x, raster.by = "Class"){
            checklist <- summary_trophic(x, info = "checklist")
            .fun_testIfIn(raster.by, c("Class","Order","Family"))
            data.mask <- unwrap(x@data.mask)
            foreach(this.taxa = unique(checklist[, raster.by])) %do% {
              cli_h2(this.taxa)
              
              taxa.uncertain <- classify(unwrap(x@data.mask), matrix(c(1,0), ncol = 2))
              taxa.count <- classify(unwrap(x@data.mask), matrix(c(1,0), ncol = 2))
              
              this.checklist <- checklist[which(checklist[ , raster.by] == this.taxa), ]
              for (this.species in this.checklist$SpeciesName) {
                cli_progress_step(this.species)
                this.occurrence <- load_data(x, SpeciesName = this.species, type = "occurrence")
                xy.uncertain <- this.occurrence %>% 
                  filter(status == "uncertain") %>% 
                  select(x,y) %>% 
                  as.matrix()
                xy.count <- this.occurrence %>% 
                  filter(inside_iucn) %>% 
                  select(x,y) %>% 
                  as.matrix()
                this.uncertain <- rasterize(xy.uncertain, data.mask, background = 0)
                this.count <- rasterize(xy.count, data.mask, background = 0)
                
                taxa.uncertain <- taxa.uncertain + this.uncertain
                taxa.count <- taxa.count + this.count
              }
              cli_progress_done()
              
              raster.uncertain.na <- taxa.uncertain/taxa.count
              raster.uncertain <- cover(raster.uncertain.na, taxa.count)
              writeRaster(raster.uncertain, paste0(x@project.name,"/summary.uncertain_",this.taxa,".tif"), overwrite = TRUE)
              NULL
            }
            invisible(NULL)
          }
)


### rasterize_prey_filtering  --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'
##' @importFrom terra global


setGeneric("rasterize_prey_filtering", def = function(x, ...) {
  standardGeneric("rasterize_prey_filtering") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom dplyr anti_join select filter
##' @importFrom terra unwrap classify cover writeRaster rasterize

setMethod('rasterize_prey_filtering', signature(x = 'trophic_dataset'),
          function(x, raster.by = "Class"){
            checklist <- summary_trophic(x, info = "checklist")
            .fun_testIfIn(raster.by, c("Class","Order","Family"))
            data.mask <- unwrap(x@data.mask)
            foreach(this.taxa = unique(checklist[, raster.by])) %do% {
              cli_h2(this.taxa)
              
              taxa.filtered <- classify(unwrap(x@data.mask), matrix(c(1,0), ncol = 2))
              taxa.count <- classify(unwrap(x@data.mask), matrix(c(1,0), ncol = 2))
              
              this.checklist <- checklist[which(checklist[ , raster.by] == this.taxa), ]
              for (this.species in this.checklist$SpeciesName) {
                cli_progress_step(this.species)
                if (length(x@kept.prey[[this.species]]) > 0) {
                  this.occurrence <- load_data(x, SpeciesName = this.species, type = "occurrence")
                  this.trophic <- load_data(x, SpeciesName = this.species, type = "trophic")
                  xy.filtered <- this.occurrence %>% 
                    filter(status == "certain") %>% 
                    select(cell, x, y) %>% 
                    anti_join(this.trophic, by = c("cell","x","y")) %>% 
                    select(x,y) %>% 
                    as.matrix()
                  xy.count <- this.occurrence %>% 
                    filter(status == "certain") %>% 
                    select(x,y) %>% 
                    as.matrix()
                  if(nrow(xy.count) > 0){
                    this.count <- rasterize(xy.count, data.mask, background = 0)
                    taxa.count <- taxa.count + this.count
                  }
                  if(nrow(xy.filtered) > 0){
                    this.filtered <- rasterize(xy.filtered, data.mask, background = 0)
                    taxa.filtered <- taxa.filtered + this.filtered
                  }
                  
                }
              }
              cli_progress_done()
              if (global(taxa.count, 'max', na.rm = TRUE) > 0) {
                raster.filtered.na <- taxa.filtered/taxa.count
                raster.filtered <- cover(raster.filtered.na, taxa.count)
                writeRaster(raster.filtered, paste0(x@project.name,"/summary.filtered_",this.taxa,".tif"), overwrite = TRUE)
              } else {
                cli_alert_warning("Skip {raster.by} {this.taxa}, that contained no predator, once prey were filtered.")
              }
              NULL
            }
            invisible(NULL)
          }
)