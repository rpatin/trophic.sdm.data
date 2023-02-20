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
##' @param list.iucn a named list of IUCN shapefile path. Name must be Amphibia_Old,
##' Amphibia_New, Mammalia_Old, Mammalia_New, Reptilia_Old, Reptilia_New, Aves.
##' @param buffer.config a named \code{list} with taxonomic groups (Class, Order,
##'   Family, Species) and associated distance that will be used to buffer IUCN
##'   range. If several taxonomic groups are valid for a given species, the
##'   lowest one will be prioritized. Buffer distances are given in km.
##' @param do.buffer a \code{boolean}. TRUE if buffered distribution 
##' needs to be calculated
##' @param do.raster a \code{boolean}. TRUE if raw distribution needs to
##' be calculated
##' @importFrom terra focal rast vect buffer aggregate rasterize

rasterize_iucn <- function(checklist, 
                           folder.iucn,
                           list.iucn,
                           data.mask,
                           project.name,
                           buffer.config,
                           nb.cpu = 1,
                           do.raster = TRUE,
                           do.buffer = TRUE){
  
  cli_h1("Rasterize and Buffer IUCN range maps")
  cli_progress_step("Argument check")
  args <- .rasterize_iucn.check.args(checklist = checklist, 
                                     folder.iucn = folder.iucn,
                                     list.iucn = list.iucn,
                                     data.mask = data.mask,
                                     project.name = project.name,
                                     buffer.config = buffer.config,
                                     nb.cpu = nb.cpu,
                                     do.raster = do.raster,
                                     do.buffer = do.buffer)
  for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)
  
  # browser()
  
  buffer.dir <- paste0(project.name, "/buffer/")
  if (!dir.exists(buffer.dir)) dir.create(buffer.dir, recursive = TRUE, showWarnings = FALSE)
  
  raster.dir <- paste0(project.name, "/raw/")
  if (!dir.exists(raster.dir)) dir.create(raster.dir, recursive = TRUE, showWarnings = FALSE)
  
  # browser()
  
  avail_code = sub(".tif","", list.files(folder.iucn))
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
  
  # browser()
  cli_progress_step("Load IUCN polygons")
  list.iucn.vect <- lapply(list.iucn, vect)
  list.iucn.df <- lapply(list.iucn.vect, as.data.frame)
  # this.species = names(listcode)[1]
  # this.species = names(listcode)[456]
  # this.species = names(listcode)[510]
  
  cli_progress_step("Running Species Loop")
  # species loop ------------------------------------------------------------
  
  foreach(this.species = names(listcode)) %dopar% {
    # browser()
    cli_progress_step(this.species)
    this.name.iucn <- listname.iucn[[this.species]]
    this.code <- listcode[[this.species]]
    this.class <- listclass[[this.species]]
    this.buffer <- buffer.config[[species.buffer[[this.species]]]]*1000
    
    # Change names of species whose name in .dbf files is given by SpeciesName
    if (this.name.iucn == "Lyciasalamandra billae ssp. billae") {
      this.name.iucn <-  "Lyciasalamandra billae"
    }
    if (this.name.iucn == "Vulpes lagopus") {
      this.name.iucn <-  "Alopex lagopus"
    }
    if (grepl("Curruca", this.name.iucn)) {
      this.name.iucn  <-  sub("Curruca", "Sylvia", this.name.iucn)
    }
    
    if (this.class != "Aves") {
      this.layer <- paste0(this.class,"_New")
      this.index <- find_iucn_index(list.iucn.df[[this.layer]],
                                    this.species,
                                    species_col = "sci_name",
                                    capitalized = FALSE)
      # If no new IUCN range available get the old one
      if (length(this.index) == 0) {
        this.layer <- paste0(this.class,"_Old")
        this.index <- find_iucn_index(list.iucn.df[[this.layer]],
                                      this.species,
                                      species_col = "BINOMIAL",
                                      capitalized = TRUE)
      }
    } else {
      this.layer <- paste0(this.class)
      this.index <- find_iucn_index(list.iucn.df[[this.layer]],
                                    this.species,
                                    species_col = "SCINAME",
                                    capitalized = TRUE)
    }
    
    if (length(this.layer) == 0) {
      .write_logfile(
        out.log = paste0(this.species, " (", this.class,
                         ") failed. Reason: no IUCN range found"),
        logfile = logfile_failed,
        project.name = project.name,
        open = "a",
        silent = TRUE)
    }
    
    this.poly <- list.iucn.vect[[this.layer]][this.index]
    
    # project and aggregate ----------------------------------------------------
    project.try <- try({
      this.poly.proj <- project(this.poly, data.mask)
      if (length(this.index) > 1) {
        this.poly <- aggregate(this.poly)
      }
    })
    if (inherits(project.try, "try-error")) {
      .write_logfile(
        out.log = paste0(this.species, " (", this.class,
                         ") failed. Project or aggregate failed"),
        logfile = logfile_failed,
        project.name = project.name,
        open = "a",
        silent = TRUE)
      return(NULL)
    }
    # rasterize ----------------------------------------------------
    if (do.raster) {
      raster.try <- try({
        this.rast <- rasterize(this.poly.proj, data.mask, background = 0)
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
        return(NULL)
      }
    }
    # buffer ----------------------------------------------------
    if (do.buffer) {
      buffer.try <- try({
        this.poly.buffered <- buffer(this.poly.proj, width = this.buffer)
        this.rast <- rasterize(this.poly.buffered, data.mask, background = 0)
        this.rast <- mask(this.rast, mask = data.mask, maskvalue = NA,
                          filename = paste0(buffer.dir, this.code, ".tif"),
                          overwrite = TRUE)
      })
      if (inherits(buffer.try, "try-error")) {
        .write_logfile(
          out.log = paste0(this.species, " (", this.class,
                           ") failed. Buffer failed"),
          logfile = logfile_failed,
          project.name = project.name,
          open = "a",
          silent = TRUE)
        return(NULL)
      }
    }
    cli_progress_done()
    .write_logfile(
      out.log = this.species,
      logfile = logfile_success,
      project.name = project.name,
      open = "a",
      silent = TRUE)
    NULL
  }
  if (has.cluster) doParallel::stopImplicitCluster()
  invisible(NULL)
}

### Argument Check -----------------------------------

.rasterize_iucn.check.args <- function(checklist, 
                                       folder.iucn,
                                       list.iucn,
                                       data.mask,
                                       project.name,
                                       buffer.config,
                                       nb.cpu,
                                       do.raster,
                                       do.buffer){
  
  
  #### checklist -------------------------------------------------------------
  .check_checklist(checklist)
  
  #### list.iucn -----------------------------------------------------------
  name.iucn.tot <- c("Amphibia_Old", "Amphibia_New", 
                     "Mammalia_Old", "Mammalia_New",
                     "Reptilia_Old", "Reptilia_New", 
                     "Aves") 
  if (!all(name.iucn.tot %in% names(list.iucn)) ||
      !all(names(list.iucn) %in% name.iucn.tot)) {
    stop("list.iucn must have name corresponding to ", paste0(name.iucn.tot, collapse = ", "),".")
  }
  
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
  #### do.raster -------------------------------------------------------  
  stopifnot(is.logical(do.raster))
  #### do.buffer -------------------------------------------------------  
  stopifnot(is.logical(do.buffer))
  
  #### buffer.config -----------------------------------------------------------
  if (do.buffer && missing(buffer.config)) {
    stop("Please provide argument buffer.config, a named list with the distance used to buffer IUCN range, by taxonomic group")
  } else {
    .fun_testIfPosNum(unlist(buffer.config))
    species.buffer <- get_species_buffer(buffer.config, checklist)
  }
  
  return(list(
    "checklist" = checklist, 
    "list.iucn" = list.iucn,
    "data.mask" = data.mask,
    "project.name" = project.name,
    "buffer.config" = buffer.config,
    "species.buffer" = species.buffer,
    "nb.cpu" = nb.cpu,
    "do.raster" = do.raster,
    "do.buffer" = do.buffer
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
##' @importFrom terra focal rast vect buffer aggregate rasterize
##' @return a named list with the buffer associated to each species

buffer_iucn <- function(checklist, 
                        folder.iucn,
                        folder.iucn.raster,
                        data.mask,
                        project.name,
                        buffer.config,
                        nb.cpu = 1){
  
  cli_h1("Buffer IUCN range maps")
  cli_progress_step("Argument check")
  args <- .buffer_iucn.check.args(checklist = checklist, 
                                  folder.iucn = folder.iucn,
                                  folder.iucn.raster = folder.iucn.raster,
                                  data.mask = data.mask,
                                  project.name = project.name,
                                  buffer.config = buffer.config,
                                  nb.cpu = nb.cpu)
  for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)

  buffer.dir <- paste0(project.name, "/buffer/")
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
  
  cli_progress_step("Running Species Loop")
  # species loop ------------------------------------------------------------
  
  foreach(this.species = names(listcode)) %dopar% {
    cli_progress_step(this.species)
    this.name.iucn <- listname.iucn[[this.species]]
    this.code <- listcode[[this.species]]
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
    }
    read.try <- try({
      this.poly <- vect(this.file)
      if (nrow(this.poly) > 1) {
        this.poly <- aggregate(this.poly)
      }
    })
    if (inherits(read.try, "try-error")) {
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
        this.rast <- rasterize(this.poly.buffered, data.mask, background = 0)
        this.rast <- mask(this.rast, mask = data.mask, maskvalue = NA,
                          filename = paste0(buffer.dir, this.code, ".tif"),
                          overwrite = TRUE)
      })
      if (inherits(buffer.try, "try-error")) {
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
    cli_progress_done()
    .write_logfile(
      out.log = this.species,
      logfile = logfile_success,
      project.name = project.name,
      open = "a",
      silent = TRUE)
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
##' @param folder.iucn.raster a path to a folder with all IUCN range as raster.
##' used to determine which species have a IUCN range.
##' @importFrom terra focal rast vect buffer aggregate rasterize
##' @return a named list with the buffer associated to each species

get_species_buffer <- function(buffer.config, checklist){
  .check_checklist(checklist)
  
  buffer.name <- names(buffer.config)
  check.taxa <- sapply(buffer.name, function(x) check_taxa(x, checklist))
  if ( !all(check.taxa) ) {
    for (this.taxa in which(!check.taxa)) {
      cli_alert_danger("taxa {buffer.name[this.taxa]} not found in the \\
                         provided checklist")
    }
    stop(paste0("Some taxa given in buffer.config were not found."))
  }
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