## --------------------------------------------------------------------------- #
# 1. sampling_effort         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name sampling_effort
##' @aliases sampling_effort-class
##' @author Remi Patin
##' 
##' @title Object class containing summary of gbif sampling effort across 
##' taxonomy (Class or Order)
##' 
##' @description Class returned by \code{\link{calc_sampling_effort}}, 
##' that calculate gbif sampling effort across several taxa, depending on argument
##' \code{sampling.effort.config}.
##' 
##' 
##' @inheritParams gbif_outsider
##' @param data.mask a \code{SpatRaster} with the extent and the grid used 
##' in the project
##' @param sampling.effort.config a named \code{list} with one element per effort
##' layer to be calculated. Contains layer name as well as their associated taxa.
##' @slot data a \code{SpatRaster} with several layers of sampling effort
##' @slot config a \code{list} with the initial configuration of the sampling
##' effort calculation (taxonomic repartition)
##' @slot species.layer a \code{list} with the layer name corresponding to each 
##' species
##' @slot checklist a \code{data.frame} with information on all species of interest
##' @slot project.name a \code{character} indicating the folder in which logfiles 
##' and data may be written
##' 
##' @examples
##' 
##' showClass("sampling_effort")
NULL

##' @name sampling_effort-class
##' @rdname sampling_effort
##' @export
##' @importFrom terra rast

## 1.1 Class Definition ----------------------------------------------------------------------------

setClass("sampling_effort",
         representation(data = 'PackedSpatRaster',
                        config = 'list',
                        species.layer = 'list',
                        checklist = "data.frame",
                        project.name = 'character'),
         validity = function(object){ 
           .check_checklist(object@checklist)
           .fun_testIfInherits(object@project.name, "character")
           .fun_testIfInherits(object@data, "PackedSpatRaster")
           .fun_testIfInherits(object@config, "list")
           .fun_testIfInherits(object@species.layer, "list")
           TRUE
         })

## 1.2 Constructors -------------------------------------------------------------

### calc_sampling_effort -----------------------------------
##' 
##' @rdname sampling_effort
##' @export
##' @inheritParams .register_cluster
##' @importFrom cli cli_progress_step cli_progress_done cli_h1 cli_alert_danger cli_h2
##' cli_process_failed cli_alert_info
##' @importFrom foreach "%do%" "%dopar%" foreach
##' @importFrom terra vect project rasterize mask wrap unwrap
##' @importFrom methods new

calc_sampling_effort <- function(checklist, folder.gbif, 
                                 sampling.effort.config,
                                 filter.atlas = FALSE, data.mask,
                                 project.name, nb.cpu = 1){
  
  cli_h1("Calculate sampling effort")
  cli_progress_step("Argument check")
  .calc_sampling_effort.check.args(
    checklist = checklist,
    folder.gbif = folder.gbif,
    sampling.effort.config = sampling.effort.config, 
    filter.atlas = filter.atlas,
    data.mask = data.mask,
    project.name = project.name,
    nb.cpu = nb.cpu
  )
  store.dir <- paste0(project.name, "/species_count/")
  if (!dir.exists(store.dir)) dir.create(store.dir, recursive = TRUE, showWarnings = FALSE)
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)
  
  ## Create species.layer ------------------------------------------------------------
  cli_progress_step('Associate each species with an effort layer')
  species.layer <- lapply(seq_len(nrow(checklist)), function(x){
    checklist.sub <- checklist[x,]
    for (level_to_check in c("SpeciesName",
                             "Family",
                             "Order",
                             "Class")){
      this.check <- check_name_config(sampling.effort.config,
                                      checklist.sub[ , level_to_check])
      if (any(this.check)){
        return(names(sampling.effort.config)[which(this.check)]) 
      }
    }
    "none"
  })
  names(species.layer) <- checklist$SpeciesName
  species.unassigned <- which(unlist(species.layer) == "none")
  if (length(species.unassigned) > 0) {
    cli_alert_danger("the following species were assigned no sampling effort layer: \\
                     {names(species.layer)[species.unassigned]}")
    stop("Please review your sampling effort configuration")
  }
  species.layer.df <- 
    data.frame(SpeciesName = names(species.layer),
               layer = unlist(species.layer))
  
  cli_progress_done()
  cli_h2("Creating each effort layer")
  # initialize logfile
  logfile <- paste0(project.name,"/log/sampling.effort.failed.log")
  if (file.exists(logfile))  file.remove(logfile)
  
  ## Effort layer Loop ------------------------------------------------------------
  if (has.cluster) cli_alert_info("Loop on species with a {nb.cpu}-core cluster")
  # browser()
  effort.layer <- foreach(this.layer = names(sampling.effort.config), .combine = 'c') %do% {
    # cli_process_failed()
    cli_progress_step("Creating effort layer '{this.layer}'")
    
    list.species <- species.layer.df$SpeciesName[species.layer.df$layer == this.layer]
    ## Species loop ------------------------------------------------------------
    this.effort.list <- foreach(this.species = list.species) %dopar% {
      # cli_progress_step(this.species)
      this.output <- load_gbif_data(species.name = this.species,
                                    folder.gbif = folder.gbif, 
                                    filter.atlas = filter.atlas)
      this.code <- checklist$Code[which(checklist$SpeciesName == this.species)]
      if (!inherits(this.output,"data.frame")) {
        .write_logfile(
          out.log = paste0("Species ", this.species, " failed. Reason: ", this.output),
          logfile = "sampling.effort.failed.log",
          project.name = project.name,
          open = "a",
          silent = TRUE
        )
        return(NULL)
      } else {
        this.output.vect <- vect(this.output,
                                 geom = c("X","Y"),
                                 crs = "+proj=longlat +datum=WGS84 +no_defs")
        this.output.vect <- project(this.output.vect, data.mask)
        this.output.rast <- 
          rasterize(this.output.vect, data.mask,
                    background = 0, fun = length)
        thisfilename = 
          paste0(store.dir,
                 this.code,
                 "_count.tif")
        this.output.rast <- mask(this.output.rast,
                                 mask = data.mask,
                                 maskvalues = NA,
                                 filename = thisfilename,
                                 overwrite = TRUE,
                                 wopt = list(names = this.code))
        # terra::plot(this.output.rast, colNA = "blue")
        # cli_progress_done()
        return(wrap(this.output.rast))
      }
    }
    this.effort.list <- sapply(this.effort.list, unwrap)
    this.effort.rast <- do.call('c', this.effort.list)
    if (inherits(this.effort.rast, 'list')) {
      this.effort.rast <- do.call('c', this.effort.rast)
    }
    cli_progress_done()
    thisfilename <- paste0(project.name, "/sampling.effort.", this.layer, ".tif")
    this.effort.rast.sum <- sum(this.effort.rast,
                                filename = thisfilename,
                                na.rm = TRUE,
                                overwrite = TRUE,
                                wopt = list(names = this.layer))
    
    # terra::plot(this.effort.rast.sum, colNA = "lightblue")
    this.effort.rast.sum
  }
  cli_progress_done()
  
  if (has.cluster) doParallel::stopImplicitCluster()
  
  ## Output ------------------------------------------------------------
  output <- new("sampling_effort")
  output@data <- wrap(effort.layer)
  output@config <- sampling.effort.config
  output@checklist <- checklist
  output@project.name <- project.name
  output@species.layer <-  species.layer
  output
}

### Argument Check -----------------------------------

.calc_sampling_effort.check.args <- function(checklist, folder.gbif, 
                                             sampling.effort.config, 
                                             filter.atlas, data.mask,
                                             project.name, nb.cpu){
  .check_checklist(checklist)
  .fun_testIfInherits(folder.gbif, "character")
  .fun_testIfDirExists(folder.gbif)
  .fun_testIfPosInt(nb.cpu)
  
  if (missing(filter.atlas)) filter.atlas <- FALSE
  stopifnot(is.logical(filter.atlas))
  
  #### check config ------------------------------------------------------------
  config.argname <- deparse(substitute(sampling.effort.config))
  
  config.name <- names(sampling.effort.config)
  if (is.null(names(sampling.effort.config))) {
    stop(paste0(config.argname, " must be a named list"))
  } 
  if (any(duplicated(config.name))) {
    stop(paste0(config.argname, " must have unique names: ",
                config.name[duplicated(config.name)],
                " are duplicated"))
  }
  # config.content <- do.call('c', sampling.effort.config)
  
  check.taxa.list <- lapply(sampling.effort.config, function(x){
    sapply(x, function(y) check_taxa(y, checklist))
  })
  
  check.taxa <- sapply(check.taxa.list, function(x) any(x))
  if ( !all(check.taxa) ) {
    for (this.taxa in which(!check.taxa)) {
      this.check <- check.taxa.list[[this.taxa]]
      cli_alert_danger("Configuration failed for {names(check.taxa.list)[this.taxa]}: \\
                       taxa {sampling.effort.config[[this.taxa]][which(!this.check)]} not found")
    }
    stop(paste0("Some taxa given in ", config.argname, " were not found."))
  }
  if (any(duplicated(unlist(sampling.effort.config)))) {
    stop(paste0("Some taxa given in ", config.argname, " were duplicated."))
  }
  return(TRUE)
}

## 1.3 Methods -------------------------------------------------------------
### show.sampling_effort    --------------------------------------------------
##' 
##' @rdname sampling_effort
##' @importMethodsFrom methods show
##' @param object an object of class \code{sampling_effort}
##' @export
##' @importFrom terra nlyr
##' 

setMethod('show', signature('sampling_effort'),
          function(object)
          {
            cat("\n Project: ",object@project.name)
            cat("\n",  nlyr(rast(object@data)), 
                " sampling effort layers")
            cat(" for ", length(object@species.layer), "species")
            cat("\n\nConfiguration of layers:\n")
            show(object@config)
            # object.range <- sapply(object@data, function(x){
            #    range(values(x), na.rm = TRUE)
            #  })
            #  colnames(object.range) <- names(object@data)  
            #  rownames(object.range) <- c("min","max")
            
            invisible(NULL)
          })
