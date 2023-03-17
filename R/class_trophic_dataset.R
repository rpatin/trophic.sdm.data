## --------------------------------------------------------------------------- #
# 1. trophic_dataset         ---------------------------------------------------
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
##'   \item protected species summary
##'   \item patrimonial species summary
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
##' the EU habitat directive
##' @slot patrimonial a \code{data.frame} with a summary of species listed as 
##' patrimonial species by the french INPN

## 1.1 Class Definition ----------------------------------------------------------------------------

setClass("trophic_summary",
         representation(occurrence = "data.frame",
                        trophic = "data.frame",
                        prey = "data.frame",
                        filtered = "data.frame",
                        protected = "data.frame",
                        patrimonial = "data.frame"),
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
##'   \item Raw trophic files (focal species and its prey)
##'   \item Final trophic files (final subsampled dataset)
##'   }
##'
##' @slot occurrence a named \code{vector} with a path to raw 
##' occurrence data for each species.
##' @slot trophic a named \code{vector} with a path to filtered
##' trophic data for each species.
##' @slot trophic.raw a named \code{vector} with a path to raw
##' trophic data for each species.

## 2.1 Class Definition ----------------------------------------------------------------------------

setClass("trophic_files",
         representation(occurrence = "character",
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

##' @name trophic_dataset
##' @aliases trophic_dataset-class
##' @author Remi Patin
##'
##' @title Dataset for running trophic SDM
##'
##' @description Class returned by \code{\link{prepare_dataset_gbif}} and
##'   \code{\link{prepare_dataset_iucn}}, that prepare a dataset for a trophic
##'   SDM, based on a gbif or IUCN workflow.
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @inheritParams rasterize_iucn
##' @param sampling.effort a \code{\link{sampling_effort}} object, generated by
##'   \code{\link{calc_sampling_effort}} containing several layers of sampling
##'   effort based on taxonomic group and the association between species and
##'   the sampling effort layer
##' @param min.gbif a \code{numeric} (\emph{default} \code{25}), the minimum
##'   number of gbif occurrences below which a species is filtered.
##' @param folder.iucn.buffer a \code{character}, folder in which IUCN buffered range
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
##' @param ... additional parameter given to \code{\link{filter_dataset}} to
##'   filter the dataset at the end of the workflow.
##' @param min.presence a \code{integer} used as a filtering parameter. Any 
##' species with less presence than \code{min.presence} in its trophic dataset
##' is removed.
##' @param min.absence a \code{integer} used as a filtering parameter. Any 
##' species with less absence than \code{min.absence} in its trophic dataset
##' is removed.
##' @param subsample.min.prevalence a \code{numeric} used as a subsampling
##'  parameter. Used to subsample absences in trophic dataset with prevalence below 
##'  \code{subsample.min.prevalence}.
##' @param subsample.max.absence a \code{numeric} used as a subsampling
##'  parameter. Used to subsample absences in trophic dataset with more absences 
##'  than \code{subsample.max.absence}.
##' @param min.prevalence.prey a \code{numeric} used as a filtering parameter. Any 
##' prey  with a prevalence in predator presence cell below 
##' \code{min.prevalence.prey}  is removed from the predator dataset.
##' @param min.absence.prey a \code{integer} used as a filtering parameter. Any 
##' prey less absence than \code{min.absence.prey} is removed from the predator
##'  dataset.
##' @slot summary.occurrence a \code{data.frame} with the list of species and 
##' the total number of presence and absence in the occurrence dataset extracted
##' from gbif and independently of prey cell selection.
##' @slot summary.predator a \code{data.frame} with a summary of the actual 
##' trophic dataset, i.e. list of species and the total number of presence 
##' and absence inside/outside IUCN range as well as additional information 
##' such as the number of prey.
##' @slot summary.prey a \code{data.frame} with a summary of occurrence and 
##' prevalence for all prey associated to a predator dataset.
##' @slot file.occurrence.link a named \code{vector} with a path to raw 
##' occurrence data for each species.
##' @slot file.trophic.link  a named \code{vector} with a path to filtered
##' trophic data for each species.
##' @slot metaweb.filtered a \code{data.frame} with all species interactions 
##' that remain after filtering of species and prey.
##' @slot checklist.filtered a \code{data.frame} with information on all species 
##'   that remain after filtering of species.
##' @slot kept.species a \code{character} vector with all species kept
##' @slot filtered.species a named \code{list} vector with all species removed 
##' as well as the motivation of removal
##' @slot kept.prey a named \code{list} with the prey kept for each species
##' @slot filtered.prey a named \code{list} vector with the prey removed for 
##' each species as well as the motivation of removal
##' @slot param.raw a \code{list} with information to regenerate the raw 
##' unfiltered dataset. 
##' @slot param a \code{list} with all parameters used to generate the dataset
##' @slot datatype a \code{character} determining the origin of the dataset:
##' either 'gbif' or 'iucn'.
##' @slot project.name a \code{character} indicating the folder in which
##'   logfiles and data may be written
##' @slot sampling.effort a \code{sampling_effort} object describing the sampling
##' effort associated to all taxa.
##' @slot data.mask a \code{SpatRaster} with the extent and the grid used 
##' in the project
##' 
##' @details 
##' \describe{
##'   \item{gbif dataset (\code{prepare_gbif_dataset})}{ Workflow to prepare gbif
##'   dataset}
##'   \item{IUCN dataset (\code{prepare_iucn_dataset})}{ Workflow to prepare iucn
##'   dataset}
##'   }
##' @examples
##'
##' showClass("trophic_dataset")
NULL

##' @name trophic_dataset-class
##' @rdname trophic_dataset
##' @export
##' @importFrom terra rast

## 1.1 Class Definition ----------------------------------------------------------------------------


setClass("trophic_dataset",
         representation(summary.occurrence = "data.frame",
                        summary.predator = "data.frame",
                        summary.prey = "data.frame",
                        summary.filter = "data.frame",
                        file.occurrence.link = "character",
                        file.trophic.link = "character",
                        metaweb.filtered = "data.frame",
                        checklist.filtered = "data.frame",
                        kept.species = "character",
                        filtered.species = "character",
                        kept.prey = "list",
                        filtered.prey = "list",
                        param.raw = "list",
                        param = "list", 
                        datatype = "character",
                        project.name = "character",
                        sampling.effort = "sampling_effort",
                        data.mask = "PackedSpatRaster"),
         validity = function(object){
           # .check_checklist(object@checklist)
           .check_checklist(object@checklist.filtered)
           .check_metaweb(object@metaweb)
           .check_metaweb(object@metaweb.filtered)
           .fun_testIfInherits(object@file.occurrence.link, "character")
           .fun_testIfInherits(object@file.trophic.link, "character")
           .fun_testIfInherits(object@kept.species, "character")
           .fun_testIfInherits(object@filtered.species, "character")
           .fun_testIfInherits(object@param, "list")
           .fun_testIfIn(obejct@datatype, c("iucn","gbif"))
           .fun_testIfInherits(object@data.mask, "PackedSpatRaster")
           .fun_testIfInherits(object@project.name, "character")
           if (!all(c("summary.occurrence.raw",
                      "summary.predator.raw",
                      "summary.prey.raw",
                      "summary.filter.raw",
                      "file.trophic.raw.link",
                      "metaweb",
                      "checklist",
                      "kept.species.raw",
                      "filtered.species.raw",
                      "kept.prey.raw",
                      "filtered.prey.raw") %in% names(param.raw))) {
             stop("summary.raw is missing elements")
           }
           TRUE
         })


## 1.2 Methods -------------------------------------------------------------
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
            n.removed <- length(do.call('c', object@filtered.prey))
            n.kept <- length(do.call('c', object@kept.prey))
            n.tot <- n.removed + n.kept
            percent.rm <- round(n.removed/(n.tot)*100, digits = 2)
            param <- object@param
            param.filter <- param$param.filter
            cli_h2("Trophic dataset Summary")
            cli_li("Project: {object@project.name}")
            cli_li("Species kept: {length(object@kept.species)}")
            cli_li("Species removed: {length(object@filtered.species)}")
            cli_li("Species interactions kept: {n.kept}")
            cli_li("Species interactions removed: {percent.rm}%")
            
            cli_h2("Parameters")
            
            cli_h3("Occurrence selection")
            cli_li("Quantile used for evaluating absences as certain = {unique(param$quantile.absence.certain)}")
            
            cli_h3("Species Filtering")
            cli_li("Minimum gbif occurrences = {param$min.gbif}")
            cli_li("Minimum presence in final dataset = {param.filter$min.presence}")
            cli_li("Minimum absence in final dataset = {param.filter$min.absence}")
            
            cli_h3("Prey filtering")
            cli_li("Minimum prey absence  = {param.filter$min.absence.prey}")
            cli_li("Minimum prey prevalence in predator presence cells  = {param.filter$min.prevalence.prey}")
            
            cli_h3("Predator cell selection")
            cli_li("Required proportion of certain prey occurrences = {param$prop.prey.certain*100}%")
            cli_li("Values for uncertain prey cells = {param$uncertain.value}")
            
            cli_h3("Subsampling absences")
            if(param.filter$subsample.method == "none"){
              cli_li("No subsampling of absences")
            } else {
              cli_li("Subsampling method = {param.filter$subsample.method}")
              cli_li("Minimum number of absence outside IUCN = {param.filter$subsample.min.absence.outside}")
              cli_li("Maximum number of absence outside IUCN = {param.filter$subsample.max.absence.outside}")
              cli_li("Aimed proportion of absence outside IUCN = {param.filter$subsample.prop.outside}")
            }
            
            invisible(NULL)
          })




### filter_species    ------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##' @importFrom dplyr select all_of
##'


setGeneric("filter_species", def = function(x, ...) {
  standardGeneric("filter_species") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_alert_success
setMethod('filter_species', signature(x = 'trophic_dataset'),
          function(x,
                   min.presence,
                   min.absence) {
            # browser()
            ### Argument check and setup --------------------------------------
            cli_h2("Parameter check")
            args <- .filter_dataset.check.args(type = "filter_species",
                                               min.presence = min.presence,
                                               min.absence = min.absence)
            
            for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
            
            x@param$param.filter$min.presence <- min.presence
            x@param$param.filter$min.absence <- min.absence
            
            # setup dir
            project.name <- x@project.name
            folder.names <- set_folder(project.name)
            for (argi in names(folder.names)) { assign(x = argi, value = folder.names[[argi]]) }
            
            # code-name converter
            listcode.full <- x@param.raw$checklist$Code
            names(listcode.full) <- x@param.raw$checklist$SpeciesName
            listname.full <- x@param.raw$checklist$SpeciesName
            names(listname.full) <- x@param.raw$checklist$Code
            
            ### Filtering Species ----------------------------------------------
            if (min.presence > 0 || min.absence > 0) {
              cli_h2("Filter species")
              cli_alert_info("Remove species with less then {min.presence} presence \\
                     or less than {min.absence} absence")
              # browser()
              summary.occurrence <- x@summary.predator %>% 
                filter(presence < min.presence |
                         absence_tot < min.absence)
              if (nrow(summary.occurrence) == 0) {
                cli_alert_success("No species to remove")
              } else {
                for (this.species in summary.occurrence$SpeciesName) {
                  cli_progress_step("Removing {this.species}")
                  # browser()
                  this.reason <- "Not enough data after rasterization and prey filtering"
                  names(this.reason) <- this.species
                  this.code <- listcode.full[this.species]
                  
                  x@summary.occurrence <- 
                    x@summary.occurrence %>% 
                    filter(SpeciesName != this.species)
                  
                  x@summary.predator <- 
                    x@summary.predator %>% 
                    filter(SpeciesName != this.species)
                  # browser()
                  # Remove species in predator dataset
                  if ( nrow(x@summary.prey) == 0 ) {
                    summary.prey.rm <- data.frame()
                  } else {
                    summary.prey.rm <-  
                      x@summary.prey %>% 
                      filter(Prey_Code == this.code)
                  }
                  if (nrow(summary.prey.rm) > 0) {
                    for (this.pred in summary.prey.rm$SpeciesName) {
                      # browser()
                      this.trophic <- fread(x@file.trophic.link[this.pred])
                      this.trophic <- 
                        this.trophic %>%
                        select(-all_of(this.code))
                      this.index <- which(x@summary.predator$SpeciesName == this.pred)
                      x@summary.predator$nprey[this.index] <- 
                        x@summary.predator$nprey[this.index] - 1
                      this.file <- paste0(dataset.dir,listcode.full[this.pred],".csv.gz")
                      fwrite(this.trophic, file = this.file)  
                      x@file.trophic.link[this.pred] <- this.file
                      x@kept.prey[[this.pred]] <- 
                        x@kept.prey[[this.pred]][
                          x@kept.prey[[this.pred]] != this.species
                        ]
                      this.prey.reason <- "Not enough data after rasterization"
                      names(this.prey.reason) <- this.species
                      x@filtered.prey[[this.pred]] <- 
                        c(x@filtered.prey[[this.pred]], this.prey.reason)
                      
                    }
                    # browser()
                    x@summary.prey <- 
                      x@summary.prey %>% 
                      filter(Code != this.code,
                             Prey_Code != this.code)
                    x@metaweb.filtered <- 
                      x@metaweb.filtered %>% 
                      filter(Pred_Code_new != this.code,
                             Prey_Code_new != this.code)
                  }
                  x@checklist.filtered <- 
                    x@checklist.filtered %>% 
                    filter(Code != this.code)
                  x@kept.species <- x@kept.species[x@kept.species != this.species]
                  x@filtered.species <- c(x@filtered.species, this.reason)
                  
                  x@file.occurrence.link <- 
                    x@file.occurrence.link[-which(names(x@file.occurrence.link) == this.species)]
                  x@file.trophic.link <-
                    x@file.trophic.link[-which(names(x@file.trophic.link) == this.species)]
                  
                  cli_progress_done()
                }
                cli_alert_success("{nrow(summary.occurrence)} species removed")
              }
            } else {
              cli_alert_success("No species filtering")
            }
            saveRDS(x, file = paste0(x@project.name,"/",x@project.name,".trophic_dataset.rds"))
            x
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
                   subsample.method,
                   subsample.regions,
                   subsample.min.absence.outside,
                   subsample.max.absence.outside,
                   subsample.prop.outside) {
            # browser()
            ### Argument check and setup --------------------------------------
            args <- .filter_dataset.check.args(
              type = "subsample",
              subsample.method = subsample.method,
              subsample.regions = subsample.regions,
              subsample.min.absence.outside = subsample.min.absence.outside,
              subsample.max.absence.outside = subsample.max.absence.outside,
              subsample.prop.outside = subsample.prop.outside)
            
            for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
            
            # setup param
            x@param$param.filter$subsample.method <- subsample.method
            x@param$param.filter$subsample.regions <- subsample.regions
            x@param$param.filter$subsample.min.absence.outside <- subsample.min.absence.outside
            x@param$param.filter$subsample.max.absence.outside <- subsample.max.absence.outside
            x@param$param.filter$subsample.prop.outside <- subsample.prop.outside
            
            # setup dir
            project.name <- x@project.name
            folder.names <- set_folder(project.name)
            for (argi in names(folder.names)) { assign(x = argi, value = folder.names[[argi]]) }
            
            # code-name converter
            listcode.full <- x@param.raw$checklist$Code
            names(listcode.full) <- x@param.raw$checklist$SpeciesName
            listname.full <- x@param.raw$checklist$SpeciesName
            names(listname.full) <- x@param.raw$checklist$Code
            
            ### Subsampling ----------------------------------------------------
            if (subsample.method != "none") {
              cli_h2("Subsample absences")
              cli_alert_info("Subsample absences to have a minimum < {subsample.min.absence.outside} \\
                     a target of {subsample.prop.outside} times the data inside IUCN range and a maximum of {subsample.max.absence.outside} absences outside IUCN range")
              # browser()
              
              listcode <- x@param.raw$checklist$Code
              names(listcode) <- x@param.raw$checklist$SpeciesName
              
              summary.predator <- x@summary.predator  %>% 
                mutate(
                  tot_inside = presence + absence_inside,
                  aim_outside =
                    pmin(
                      pmax(subsample.min.absence.outside,
                           tot_inside*subsample.prop.outside),
                      subsample.max.absence.outside
                    )
                )  %>% 
                filter(absence_outside > aim_outside)
              
              if (nrow(summary.predator) == 0) {
                cli_alert_success("No species require subsampling")
              } else {
                this.species <- "Ardea cinerea"
                for (this.species in summary.predator$SpeciesName) {
                  cli_progress_step(this.species)
                  this.raw <- load_data(x, SpeciesName = this.species, type = "trophic.raw")
                  this.index <- which(summary.predator$SpeciesName == this.species)
                  this.aim <- summary.predator$aim_outside[this.index]
                  this.nprey <- summary.predator$nprey[this.index]
                  this.code <- listcode[this.species]
                  if (subsample.method == "random") {
                    which.absences.outside <- which(this.raw$presence == 0 & !this.raw$inside_iucn)
                    this.to.remove <- length(which.absences.outside) - this.aim
                    which.to.remove <- sample(which.absences.outside, 
                                              replace = FALSE,
                                              size = this.to.remove)
                    this.raw$subsample <- TRUE
                    this.raw$subsample[which.to.remove] <- FALSE
                    fwrite(this.raw, file = x@param.raw$file.trophic.raw.link[this.species])
                    
                    this.trophic <- filter(this.raw, subsample)
                    this.filtered.prey <- listcode[names(x@filtered.prey[[this.species]])]
                    this.to.remove <- which(this.filtered.prey %in% colnames(this.trophic))
                    if (length(this.to.remove) > 0) {
                      for (this.prey in this.to.remove) {
                        this.trophic[,this.filtered.prey[this.prey]] <- NULL
                      }
                    }
                    file.trophic <- paste0(project.name, "/trophic_dataset/", this.code,".csv.gz")
                    fwrite(this.trophic, file = file.trophic)
                    x@file.trophic.link[this.species] <- file.trophic
                    x@summary.predator <- 
                      filter(x@summary.predator, SpeciesName != this.species) %>% 
                      rbind(get_predator_summary(this.trophic))
                    if (this.nprey > 0) {
                      x@summary.prey <- 
                        filter(x@summary.prey, SpeciesName != this.species) %>% 
                        rbind(get_prey_summary(this.trophic))
                    }                   
                  }
                  cli_progress_done()
                }
              }
              cli_alert_success("All species subsampled")
            }
            saveRDS(x, file = paste0(x@project.name,"/",x@project.name,".trophic_dataset.rds"))
            x
          })

### filter_prey  ------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'


setGeneric("filter_prey", def = function(x, ...) {
  standardGeneric("filter_prey") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_alert_success
setMethod('filter_prey', signature(x = 'trophic_dataset'),
          function(x,
                   min.prevalence.prey,
                   min.absence.prey) {
            # browser()
            ### Argument check and setup --------------------------------------
            cli_h2("Parameter check")
            args <- .filter_dataset.check.args(
              type = "filter_prey",
              min.prevalence.prey = min.prevalence.prey,
              min.absence.prey = min.absence.prey)
            
            for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
            
            x@param$param.filter$min.prevalence.prey <- min.prevalence.prey
            x@param$param.filter$min.absence.prey <- min.absence.prey
            
            # setup dir
            project.name <- x@project.name
            folder.names <- set_folder(project.name)
            for (argi in names(folder.names)) { assign(x = argi, value = folder.names[[argi]]) }
            
            # code-name converter
            listcode.full <- x@param.raw$checklist$Code
            names(listcode.full) <- x@param.raw$checklist$SpeciesName
            listname.full <- x@param.raw$checklist$SpeciesName
            names(listname.full) <- x@param.raw$checklist$Code
            
            ### Filtering Prey -------------------------------------------------
            
            if (min.prevalence.prey > 0 || min.absence.prey > 0) {
              cli_h2("Filter prey")
              cli_alert_info("Remove prey with less then {min.absence.prey} absence \\
                     or a prevalence in predator presence less below {min.prevalence.prey}")
              summary.prey.rm <- 
                x@summary.prey %>% 
                filter(prevalence_pred1 < min.prevalence.prey |
                         absence < min.absence.prey)
              
              if (nrow(summary.prey.rm) == 0) {
                cli_alert_success("No predator-prey interactions to remove")
              } else {
                for (this.pred in unique(summary.prey.rm$SpeciesName)) {
                  this.summary <- filter(summary.prey.rm, SpeciesName == this.pred)
                  preycode.to.rm <- this.summary$Prey_Code
                  prey.to.rm <- listname.full[preycode.to.rm]
                  
                  this.trophic <- fread(x@file.trophic.link[this.pred])
                  for (this.prey in preycode.to.rm) {
                    this.trophic[, this.prey] <- NULL
                    this.index <- which(x@summary.predator$SpeciesName == this.pred)
                    x@summary.predator$nprey[this.index] <- 
                      x@summary.predator$nprey[this.index] - 1
                  }
                  this.file <- paste0(dataset.dir,listcode.full[this.pred],".csv.gz")
                  fwrite(this.trophic, file = this.file)  
                  x@file.trophic.link[this.pred] <- this.file
                  x@kept.prey[[this.pred]] <- 
                    x@kept.prey[[this.pred]][
                      !x@kept.prey[[this.pred]]  %in% prey.to.rm
                    ]
                  this.prey.reason <- rep("Low prey prevalence in pred. range or low prey absence count",
                                          length(prey.to.rm))
                  names(this.prey.reason) <- prey.to.rm
                  x@filtered.prey[[this.pred]] <- 
                    c(x@filtered.prey[[this.pred]], this.prey.reason)
                }
                
                x@summary.prey <- 
                  x@summary.prey %>% 
                  filter(prevalence_pred1 >= min.prevalence.prey,
                         absence >= min.absence.prey)
                
                x@metaweb.filtered <- 
                  x@metaweb.filtered %>% 
                  filter(Pred_Code_new %in% x@summary.prey$Code &
                           Prey_Code_new %in% x@summary.prey$Prey_Code)
                cli_alert_success("{nrow(summary.prey.rm)} predator-prey interactions successfully removed")
              }
            } else {
              cli_alert_success("No prey filtering")
            }
            saveRDS(x, file = paste0(x@project.name,"/",x@project.name,".trophic_dataset.rds"))
            x
          })

#### Argument Check -----------------------------------

.filter_dataset.check.args <- function(type,
                                       min.presence,
                                       min.absence,
                                       subsample.method,
                                       subsample.regions,
                                       subsample.min.absence.outside,
                                       subsample.max.absence.outside,
                                       subsample.prop.outside,
                                       min.prevalence.prey,
                                       min.absence.prey){
  
  # Type
  if (missing(type)) {
    type <- "all"
  } else {
    .fun_testIfIn(type, c("all",
                          "filter_species",
                          "filter_prey",
                          "subsample"))
  }
  
  
  ##### arg check filter_species ------------------------------------------------
  if (type %in% c("filter_species", "all")) {
    
    if (missing(min.presence)) {
      min.presence <- 25
      cli_alert_info("Set default min.presence = 25")
    } else {
      .fun_testIfPosInt(min.presence)
    }
    
    if (missing(min.absence)) {
      min.absence <- 25
      cli_alert_info("Set default min.absence = 25")
    } else {
      .fun_testIfPosInt(min.absence)
    }
  } else {
    min.absence <- NA
    min.presence <- NA
  }
  
  
  ##### arg check subsample ------------------------------------------------
  if (type %in% c("subsample", "all")) {
    if (missing(subsample.method)) {
      subsample.method <- "none"
    } else {
      .fun_testIfIn(subsample.method, c("none",
                                        "random",
                                        "stratified"))
    }
    if (subsample.method == "none") {
      subsample.min.absence.outside <- NA
      subsample.max.absence.outside <- NA
      subsample.prop.outside <- NA
      subsample.regions <- NA
    } else {
      if (missing(subsample.min.absence.outside)) {
        subsample.min.absence.outside <- 3000
      } else {
        .fun_testIfPosInt(subsample.min.absence.outside)
      }
      
      if (missing(subsample.max.absence.outside)) {
        subsample.max.absence.outside <- 15000
      } else {
        .fun_testIfPosInt(subsample.max.absence.outside)
        stopifnot(subsample.max.absence.outside > subsample.min.absence.outside)
      }
      
      subsample.max.absence.outside
      if (missing(subsample.prop.outside)) {
        subsample.prop.outside <- 3
      } else  {
        .fun_testIfPosInt(subsample.prop.outside)
      } 
      if (subsample.method == "stratified") {
        if (missing(subsample.regions)) {
          stop("subsample.regions is required to run stratified subsampling")
        } else {
          .fun_testIfInherits(subsample.regions, "SpatVector")
        }
      } else {
        subsample.regions <- NA
      }
    }
  } else {
    subsample.method <- "none"
    subsample.regions <- NA
    subsample.prop.outside <- NA
    subsample.min.absence.outside <- NA
    subsample.max.absence.outside <- NA
  }
  ##### arg check filter_prey ------------------------------------------------
  if (type %in% c("filter_prey", "all")) {
    if (missing(min.prevalence.prey)) {
      min.prevalence.prey <- 0
    } else {
      .fun_testIfPosNum(min.prevalence.prey)
    }
    
    if (missing(min.absence.prey)) {
      min.absence.prey <- 0
    } else {
      .fun_testIfPosInt(min.absence.prey)
    }
  } else {
    min.absence.prey <- NA
    min.prevalence.prey <- NA
  }
  return(list(min.presence = min.presence,
              min.absence = min.absence,
              subsample.method = subsample.method,
              subsample.regions = subsample.regions,
              subsample.min.absence.outside = subsample.min.absence.outside,
              subsample.max.absence.outside = subsample.max.absence.outside,
              subsample.prop.outside = subsample.prop.outside,
              min.prevalence.prey = min.prevalence.prey,
              min.absence.prey = min.absence.prey))
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
                  this.df <- fread(this.file)
                  this.df$subsample <- TRUE
                  fwrite(this.df, this.file)
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
            
            saveRDS(x, file = paste0(x@project.name,"/",x@project.name,".trophic_dataset.rds"))
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
                               "summary.predator",
                               "summary.prey",
                               "metaweb",
                               "checklist",
                               "kept.species",
                               "kept.prey",
                               "filtered.species",
                               "filtered.prey")
            .fun_testIfIn(info, possible_info)
            
            if (info == "summary.occurrence") {
              return(x@summary.occurrence)
            } else if (info == "summary.uncertain") {
              output <- x@summary.occurrence %>% 
                mutate(tot.iucn =
                         absence.inside.certain + 
                         absence.inside.uncertain +
                         presence,
                       prop.presence = presence/tot.iucn,
                       prop.certain = absence.inside.certain/tot.iucn,
                       prop.uncertain = absence.inside.uncertain/tot.iucn) %>% 
                left_join(x@checklist.filtered, by = c("SpeciesName","Code"))
              return(output)
            } else if (info == "summary.predator") {
              return(x@summary.predator)
            } else if (info == "summary.prey") {
              return(x@summary.prey)
            } else if (info == "summary.filter") {
              return(x@summary.filter)
            } else if (info == "metaweb") {
              return(x@metaweb.filtered)
            } else if (info == "checklist") {
              return(x@checklist.filtered)
            } else if (info == "filtered.species") {
              
              tmp <- x@filtered.species
              output <- data.frame(SpeciesName = names(tmp),
                                   reason = unname(tmp))
              return(output)
              
            } else if (info == "filtered.prey") {
              tmp.prey <- x@filtered.prey
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
              return(x@kept.species)
              return(output)
            } else if (info == "kept.prey") {
              return(x@kept.prey)
            }
          })

### plot_uncertain    --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'


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
            checklist <- dataset.full@checklist.filtered
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
              this.df <- load_data(dataset.full, SpeciesName = this.species, type = "occurrence") %>% 
                filter(inside_iucn) %>% 
                mutate(datatype = ifelse(presence == 1, "presence", paste0(status, " absence")))
              g <- ggplot() +
                geom_spatraster(data = mutate(data.mask, mask.grid = as.character(mask.grid))) +
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
            checklist <- dataset.full@checklist.filtered
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
              
              this.prey.filtered <- length(x@filtered.prey[[this.species]])
              this.prey.kept <- length(x@kept.prey[[this.species]])
              this.prey <- this.prey.filtered + this.prey.kept
              if (this.prey > 0) {
                
                
                this.Order <- listOrder[this.species]
                this.Class <- listClass[this.species]
                this.summary <- filter(summary.uncertain, SpeciesName == this.species)
                this.file <- paste0(plot.folder, this.Class, "_", this.Order, "_", this.species, ".png")
                
                this.occurrence <- load_data(dataset.full, 
                                             SpeciesName = this.species,
                                             type = "occurrence") 
                this.df <- 
                  this.occurrence %>% 
                  filter(inside_iucn) %>% 
                  mutate(datatype = ifelse(presence == 1, "presence", paste0(status, " absence")))
                this.trophic <- load_data(dataset.full, 
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
                  geom_spatraster(data = mutate(data.mask, mask.grid = as.character(mask.grid))) +
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
            checklist <- dataset.full@checklist.filtered
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
              summary.trophic <- summary_trophic(x, info = "summary.predator")  
            } else {
              summary.trophic <- x@param.raw$summary.predator.raw
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
                load_data(dataset.full, 
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
                geom_spatraster(data = mutate(data.mask, mask.grid = as.character(mask.grid))) +
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
##' @export

setMethod('rasterize_uncertain', signature(x = 'trophic_dataset'),
          function(x, raster.by = "Class"){
            checklist <- x@checklist.filtered
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


setGeneric("rasterize_prey_filtering", def = function(x, ...) {
  standardGeneric("rasterize_prey_filtering") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom dplyr anti_join select filter
##' @importFrom terra unwrap classify cover writeRaster rasterize

setMethod('rasterize_prey_filtering', signature(x = 'trophic_dataset'),
          function(x, raster.by = "Class"){
            checklist <- x@checklist.filtered
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