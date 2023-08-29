## --------------------------------------------------------------------------- #
# 1. rasterize_iucn         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name rasterize_iucn
##' @author Remi Patin
##'
##' @title Buffer IUCN distribution
##'
##' @description \code{buffer_iucn} get the IUCN polygons for each class (old 
##' and new polygons), it then project to the \code{data.mask} projection,
##' buffer the polygons with \code{buffer.config} and rasterize the final projection.
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @inheritParams .register_cluster
##' @importFrom terra focal rast vect buffer aggregate rasterize
##' @export

rasterize_iucn <- function(checklist, 
                           folder.iucn,
                           data.mask,
                           project.name,
                           nb.cpu = 1){
  
  cli_h1("Rasterize IUCN range maps")
  cli_progress_step("Argument check")
  args <- .rasterize_iucn.check.args(checklist = checklist, 
                                     folder.iucn = folder.iucn,
                                     data.mask = data.mask,
                                     project.name = project.name,
                                     nb.cpu = nb.cpu)
  for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)
  

  raster.dir <- paste0(project.name)
  if (!dir.exists(raster.dir)) dir.create(raster.dir, recursive = TRUE, showWarnings = FALSE)
  
  # browser()
  listcode <- checklist$Code
  names(listcode) <- checklist$SpeciesName
  listclass <- checklist$Class
  names(listclass) <- checklist$SpeciesName
  # init log
  logfile_failed <- "bufferIUCN_failed.log"
  path_logfile_failed <- paste0(project.name,"/log/",logfile_failed)
  if (file.exists(path_logfile_failed)) file.remove(path_logfile_failed)
  logfile_success <- "bufferIUCN_success.log"
  path_logfile_success <- paste0(project.name,"/log/",logfile_success)
  if (file.exists(path_logfile_success)) file.remove(path_logfile_success)
  
  cli_progress_step("Running Species Loop")
  # species loop ------------------------------------------------------------
  
  foreach(this.species = names(listcode)) %dopar% {
    # browser()
    this.class <- listclass[this.species]
    cli_progress_step(this.species)
    this.code <- listcode[this.species]
      this.iucn <-
        locate_iucn_distribution(species.code = this.code,
                                 folder.iucn = folder.iucn,
                                 filetype = ".shp")

    if (is.null(this.iucn)) {
      .write_logfile(
        out.log = paste0(this.species, " (", this.class,
                         ") failed. Reason: Could not retrieve IUCN information"),
        logfile = logfile_failed,
        project.name = project.name,
        open = "a",
        silent = TRUE)

      cli_process_failed()
    }  else {
      # get IUCN distribution
      this.poly <- vect(this.iucn)
      
      raster.try <- try({
        this.rast <- rasterize(this.poly, data.mask, background = 0)
        this.rast <- mask(this.rast, mask = data.mask, maskvalue = NA,
                          filename = paste0(raster.dir, this.code, ".tif"),
                          overwrite = TRUE)
      })
      
      if (inherits(raster.try, "try-error")) {
        .write_logfile(
          out.log = paste0(this.species, " (", this.class,
                           ") failed. Basic Rasterize failed"),
          logfile = logfile_failed,
          project.name = project.name,
          open = "a",
          silent = TRUE)
        cli_process_failed()
        return(NULL)
      } else {
        cli_progress_done()
        
      }
    
    .write_logfile(
      out.log = this.species,
      logfile = logfile_success,
      project.name = project.name,
      open = "a",
      silent = TRUE)
    }
    NULL
  }
  if (has.cluster) doParallel::stopImplicitCluster()
  invisible(NULL)
}

### Argument Check -----------------------------------

.rasterize_iucn.check.args <- function(checklist, 
                                       folder.iucn,
                                       data.mask,
                                       project.name,
                                       nb.cpu){
  
  
  #### checklist -------------------------------------------------------------
  .check_checklist(checklist)
  
  #### data.mask -------------------------------------------------------------
  .fun_testIfInherits(data.mask, "SpatRaster")
  
  #### project.name ----------------------------------------------------------
  .fun_testIfInherits(project.name, "character")
  if (!dir.exists(project.name)) dir.create(project.name, recursive = TRUE)
  
  #### folder.iucn -----------------------------------------------------------
  .fun_testIfInherits(folder.iucn, "character")
  .fun_testIfDirExists(folder.iucn)
  
  #### nb.cpu -------------------------------------------------------  
  .fun_testIfPosInt(nb.cpu)

  return(list(
    "checklist" = checklist, 
    "data.mask" = data.mask,
    "project.name" = project.name,
    "nb.cpu" = nb.cpu
  ))
}


## --------------------------------------------------------------------------- #
# 2. buffer_iucn         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name buffer_iucn
##' @author Remi Patin
##'
##' @title Buffer IUCN distribution
##'
##' @description \code{buffer_iucn} get the IUCN polygons for each class (old 
##' and new polygons), it then project to the \code{data.mask} projection,
##' buffer the polygons with \code{buffer.config} and rasterize the final projection.
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @inheritParams .register_cluster
##' @param buffer.config a named \code{list} with taxonomic groups (Class, Order,
##'   Family, Species) and associated distance that will be used to buffer IUCN
##'   range. If several taxonomic groups are valid for a given species, the
##'   lowest one will be prioritized. Buffer distances are given in km.
##' @param folder.iucn.raster a path to a folder with all IUCN range as raster.
##' used to determine which species have a IUCN range.
##' @param overwrite a \code{boolean}, whether previous results should be 
##' overwritten or not
##' @importFrom terra focal rast vect buffer aggregate rasterize
##' @importFrom cli cli_status_clear
##' @return a named list with the buffer associated to each species
##' 
##' @export

buffer_iucn <- function(checklist, 
                        folder.iucn,
                        folder.iucn.raster,
                        data.mask,
                        project.name,
                        buffer.config,
                        overwrite = TRUE,
                        nb.cpu = 1){
  cli_status_clear()
  cli_h1("Buffer IUCN range maps")
  cli_progress_step("Argument check")
  args <- .buffer_iucn.check.args(checklist = checklist, 
                                  folder.iucn = folder.iucn,
                                  folder.iucn.raster = folder.iucn.raster,
                                  data.mask = data.mask,
                                  project.name = project.name,
                                  buffer.config = buffer.config,
                                  overwrite = overwrite,
                                  nb.cpu = nb.cpu)
  for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)
  
  buffer.dir <- paste0(project.name)
  if (!dir.exists(buffer.dir)) dir.create(buffer.dir, recursive = TRUE, showWarnings = FALSE)
  
  avail_code = sub(".tif","", list.files(folder.iucn.raster))
  checklist.sub <- 
    checklist %>% 
    filter(Code %in% avail_code)
  
  listcode <- checklist.sub$Code
  names(listcode) <- checklist.sub$SpeciesName
  listname.iucn <- checklist.sub$NAME_IUCN
  names(listname.iucn) <- checklist.sub$SpeciesName
  listclass <- checklist.sub$Class
  names(listclass) <- checklist.sub$SpeciesName
  # init log
  logfile_failed <- "bufferIUCN_failed.log"
  path_logfile_failed <- paste0(project.name,"/log/",logfile_failed)
  if (file.exists(path_logfile_failed)) file.remove(path_logfile_failed)
  logfile_success <- "bufferIUCN_success.log"
  path_logfile_success <- paste0(project.name,"/log/",logfile_success)
  if (file.exists(path_logfile_success)) file.remove(path_logfile_success)
  
  # this.species = names(listcode)[1]
  # this.species = names(listcode)[456]
  # this.species = names(listcode)[510]
  cli_progress_done()
  cli_alert_info("Running Species Loop")
  # species loop ------------------------------------------------------------
  
  foreach(this.species = names(listcode)) %dopar% {
    this.code <- listcode[[this.species]]
    this.out.file <- paste0(buffer.dir, this.code, ".tif")
    this.name.iucn <- listname.iucn[[this.species]]
    this.class <- listclass[[this.species]]
    this.buffer <- buffer.config[[species.buffer[[this.species]]]]*1000
    this.file <- locate_iucn_distribution(this.code, folder.iucn, filetype = ".shp")
    if (length(this.file) == 0) {
      .write_logfile(
        out.log = paste0(this.species, " (", this.class,
                         ") failed. Reason: no IUCN range found"),
        logfile = logfile_failed,
        project.name = project.name,
        open = "a",
        silent = TRUE)
    } else if (!file.exists(this.out.file) | overwrite) {
      cli_progress_step(this.species)

      read.try <- try({
        this.poly <- vect(this.file)
        if (nrow(this.poly) > 1) {
          this.poly <- aggregate(this.poly)
        }
      })
      if (inherits(read.try, "try-error")) {
        cli_process_failed()
        .write_logfile(
          out.log = paste0(this.species, " (", this.class,
                           ") failed. Read or aggregate failed"),
          logfile = logfile_failed,
          project.name = project.name,
          open = "a",
          silent = TRUE)
        return(NULL)
      }
      # buffer ----------------------------------------------------
      buffer.try <- try({
        this.poly.buffered <- buffer(this.poly, width = this.buffer)
        this.rast <- rasterize(this.poly.buffered, data.mask, 
                               background = 0,
                               touches = TRUE)
        this.rast <- mask(this.rast, mask = data.mask, maskvalue = NA,
                          filename = this.out.file,
                          overwrite = TRUE)
        rm(this.poly, this.poly.buffered, this.rast); gc();
      })
      if (inherits(buffer.try, "try-error")) {
        cli_process_failed()
        .write_logfile(
          out.log = paste0(this.species, " (", this.class,
                           ") failed. Buffer failed"),
          logfile = logfile_failed,
          project.name = project.name,
          open = "a",
          silent = TRUE)
        return(NULL)
      }
      warnings()
      .write_logfile(
        out.log = this.species,
        logfile = logfile_success,
        project.name = project.name,
        open = "a",
        silent = TRUE)
      cli_progress_done()
    }
    NULL
  }
  if (has.cluster) doParallel::stopImplicitCluster()
  species.buffer
}

### Argument Check -----------------------------------

.buffer_iucn.check.args <- function(checklist, 
                                    folder.iucn,
                                    folder.iucn.raster,
                                    data.mask,
                                    project.name,
                                    buffer.config,
                                    overwrite, 
                                    nb.cpu){
  
  
  #### checklist -------------------------------------------------------------
  .check_checklist(checklist)
  
  
  #### data.mask -------------------------------------------------------------
  .fun_testIfInherits(data.mask, "SpatRaster")
  
  #### project.name ----------------------------------------------------------
  .fun_testIfInherits(project.name, "character")
  if (!dir.exists(project.name)) dir.create(project.name, recursive = TRUE)
  
  #### folder.iucn -----------------------------------------------------------
  .fun_testIfInherits(folder.iucn, "character")
  .fun_testIfDirExists(folder.iucn)
  
  #### folder.iucn.raster -----------------------------------------------------------
  .fun_testIfInherits(folder.iucn.raster, "character")
  .fun_testIfDirExists(folder.iucn.raster)
  
  #### overwrite ------------------
  
  stopifnot(is.logical(overwrite))
  
  #### nb.cpu -------------------------------------------------------  
  .fun_testIfPosInt(nb.cpu)
  
  #### buffer.config -----------------------------------------------------------
  if (missing(buffer.config)) {
    stop("Please provide argument buffer.config, a named list with the distance used to buffer IUCN range, by taxonomic group")
  } else {
    .fun_testIfPosNum(unlist(buffer.config))
    species.buffer <- get_species_buffer(buffer.config, checklist)
  }
  
  return(list(
    "checklist" = checklist, 
    "data.mask" = data.mask,
    "project.name" = project.name,
    "buffer.config" = buffer.config,
    "species.buffer" = species.buffer,
    "nb.cpu" = nb.cpu
  ))
}


## --------------------------------------------------------------------------- #
# 3. get_species_buffer         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name get_species_buffer
##' @author Remi Patin
##'
##' @title get species-buffer association
##'
##' @description \code{get_species_buffer} get the buffer corresponding to each
##'   species, based on a list with buffer distance based on taxonomy
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @inheritParams .register_cluster
##' @param buffer.config a named \code{list} with taxonomic groups (Class, Order,
##'   Family, Species) and associated distance that will be used to buffer IUCN
##'   range. If several taxonomic groups are valid for a given species, the
##'   lowest one will be prioritized. Buffer distances are given in km.
##' @importFrom terra focal rast vect buffer aggregate rasterize
##' @return a named list with the buffer associated to each species

get_species_buffer <- function(buffer.config, checklist){
  .check_checklist(checklist)
  
  buffer.name <- names(buffer.config)
  # check.taxa <- sapply(buffer.name, function(x) check_taxa(x, checklist))
  # if ( !all(check.taxa) ) {
  #   for (this.taxa in which(!check.taxa)) {
  #     cli_alert_danger("taxa {buffer.name[this.taxa]} not found in the \\
  #                        provided checklist")
  #   }
  #   stop(paste0("Some taxa given in buffer.config were not found."))
  # }
  species.buffer <- lapply(seq_len(nrow(checklist)), function(x){
    checklist.sub <- checklist[x,]
    for (level_to_check in c("SpeciesName",
                             "Family",
                             "Order",
                             "Class")) {
      this.check <- sapply(buffer.name,  function(x) x == checklist.sub[ , level_to_check])
      if (any(this.check)) {
        return(buffer.name[which(this.check)]) 
      }
    }
    "none"
  })
  names(species.buffer) <- checklist$SpeciesName
  species.unassigned <- which(unlist(species.buffer) == "none")
  if (length(species.unassigned) > 0) {
    cli_alert_danger("the following species were assigned no buffer.config \\
                       values: {names(species.buffer)[species.unassigned]}")
    stop("Please review buffer.config to have all species covered")
  }
  species.buffer
}