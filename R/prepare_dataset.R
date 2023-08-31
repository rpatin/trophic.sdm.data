
### prepare_dataset -----------------------------------
##'
##' @rdname trophic_dataset
##' @export
##' @inheritParams .register_cluster
##' @inheritParams assemble_trophic
##' @param ... additional argument transmitted to further functions
##' @param param.subsampling a \code{\link{param_subsampling}} class object
##' describing the parameterization of subsampling
##' @param use.gbif a \code{boolean}. If \code{FALSE}, use only IUCN data to 
##' build the dataset.
##' @param param.gbif a \code{\link{param_gbif}} class object describing the 
##' parameter used to extract gbif data.
##' @param overwrite.occurrence a \code{boolean}. If \code{FALSE}, only the
##' species for which the occurrence file does not exist are processed for 
##' building the occurrence dataset
##' @param overwrite.trophic a \code{boolean}. If \code{FALSE}, only the
##' species for which the trophic file does not exist are processed for 
##' building the trophic dataset
##' @importFrom terra focal rast vect extract crs
##' @importFrom dplyr rename filter semi_join select mutate first summarize join_by
##' @importFrom tidyr pivot_wider
##' @importFrom data.table fwrite fread
##' @importFrom methods validObject

prepare_dataset <- function(checklist, 
                            metaweb,
                            trophic.groups,
                            use.gbif = TRUE,
                            param.gbif,
                            param.subsampling,
                            folder.iucn,
                            data.mask,
                            project.name,
                            nb.cpu,
                            overwrite.occurrence = FALSE,
                            overwrite.trophic = FALSE,
                            subsample.assemble,
                            ...){
  
  cli_h1("Prepare trophic dataset data")
  cli_progress_step("Argument check")
  args <- .prepare_dataset.check.args(
    checklist = checklist, 
    metaweb = metaweb,
    # trophic.groups = trophic.groups,
    use.gbif = use.gbif,
    param.gbif = param.gbif,
    param.subsampling = param.subsampling,
    folder.iucn = folder.iucn,
    data.mask = data.mask, 
    project.name = project.name,
    nb.cpu = nb.cpu,
    overwrite.occurrence = overwrite.occurrence,
    overwrite.trophic = overwrite.trophic,
    subsample.assemble = subsample.assemble)
  for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
  
  ## initialize folder -----------------------------------------------------
  folder.names <- set_folder(project.name)
  for (argi in names(folder.names)) { assign(x = argi, value = folder.names[[argi]]) }
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)
  
  ## initialize variables -----------------------------------------------------
  param = new("param_trophic")
  param@checklist.raw <- checklist
  param@metaweb.raw <- metaweb
  param@trophic.groups.raw <- trophic.groups
  param@param.gbif <- param.gbif
  param@param.subsampling <- param.subsampling
  param@folder.iucn <- folder.iucn
  
  
  listcode <- checklist$Code
  names(listcode) <- checklist$SpeciesName
  # init log -------------------------------------------------
  logfile_step1 <- "Step1_speciesOccurrence.log"
  link_logfile_step1 <- paste0(project.name,"/log/",logfile_step1)
  if (file.exists(link_logfile_step1)) file.remove(link_logfile_step1)
  logfile_step2 <- "Step2_assemblePreyData.log"
  link_logfile_step2 <- paste0(project.name,"/log/",logfile_step2)
  if (file.exists(link_logfile_step2)) file.remove(link_logfile_step2)
  
  cli_progress_done()
  ## Step 1 - Generate Species Occurrence dataset ----------------------------
  cli_h2("Step 1 - Individual species occurrence dataset")
  this.species = names(listcode)[1]
  species.occurrence <- foreach(this.species = names(listcode)) %dopar% {
    this.code <- listcode[this.species]
    if (param.gbif@use.gbif) {
      gbif.output <- extract_gbif(this.species,
                                  this.code,
                                  param.gbif, 
                                  data.mask = data.mask,
                                  occurrence.dir = occurrence.dir,
                                  occurrence.rast.dir = occurrence.rast.dir,
                                  status.rast.dir = status.rast.dir,
                                  project.name = project.name,
                                  logfile = logfile_step1,
                                  overwrite = overwrite.occurrence) 
      if(activate_backup(gbif.output, param.gbif@backup.iucn)) {
        cli_alert_info("activate IUCN backup for {this.species}")
        gbif.output <- extract_iucn(this.species, 
                                   this.code, 
                                   data.mask = data.mask,
                                   folder.iucn = param@folder.iucn, 
                                   occurrence.dir = occurrence.dir,
                                   occurrence.rast.dir = occurrence.rast.dir,
                                   status.rast.dir = status.rast.dir,
                                   project.name = project.name,
                                   logfile = logfile_step1,
                                   overwrite = TRUE) # cannot properly check overwriting if gbif was just written but have not enough points
      }
      return(gbif.output)
    } else {
      iucn.output <- extract_iucn(this.species, 
                                  this.code, 
                                  data.mask = data.mask,
                                  folder.iucn = param@folder.iucn, 
                                  occurrence.dir = occurrence.dir,
                                  occurrence.rast.dir = occurrence.rast.dir,
                                  status.rast.dir = status.rast.dir,
                                  project.name = project.name,
                                  logfile = logfile_step1,
                                  overwrite = overwrite.occurrence)
      return(iucn.output)
    }
  } # end foreach Step 1
  ## Summary Step 1 - Occurrence dataset ---------
  occurrence.summary <- lapply(species.occurrence, function(x){
    if (inherits(x$occurrence.summary, "data.frame")) {
      return(x$occurrence.summary)
    } else {
      return(NULL)
    }
  }) %>% 
    rbindlist(., fill = TRUE)
  
  # browser()
  filtered.species <- 
    do.call('c', 
            lapply(species.occurrence, function(x){
              if (x$status == "failed") {
                y <- x$reason
                names(y) <- x$SpeciesName  
                return(y)
              } else {
                return(NULL)
              }
            })) 
  if (is.null(filtered.species)) filtered.species <- character()
  
  # browser()
  fresh.occurrence <- 
    do.call('c', 
            lapply(species.occurrence, function(x){
              if (x$status == "success") {
                y <- x$fresh
                names(y) <- x$SpeciesName  
                return(y)
              } else {
                return(NULL)
              }
            })) 
  # browser()
  if (is.null(filtered.species)) filtered.species <- character()
  
  # browser()
  file.occurrence.link <- 
    do.call('c', 
            lapply(species.occurrence, function(x){
              if (x$status == "failed") {
                return(NULL)
              } else {
                y <- x$occurrence.file
                names(y) <- x$SpeciesName  
                return(y)
              }
            }))
  
  # browser()
  file.occurrence.rast.link <- 
    do.call('c', 
            lapply(species.occurrence, function(x){
              if (x$status == "failed") {
                return(NULL)
              } else {
                y <- x$occurrence.rast.file
                names(y) <- x$SpeciesName  
                return(y)
              }
            }))
  
  # browser()
  file.status.rast.link <- 
    do.call('c', 
            lapply(species.occurrence, function(x){
              if (x$status == "failed") {
                return(NULL)
              } else {
                y <- x$status.rast.file
                names(y) <- x$SpeciesName  
                return(y)
              }
            }))
  
  # browser()
  IUCN.link <- 
    do.call('c', 
            lapply(species.occurrence, function(x){
              if (x$status == "failed" || x$IUCN.file == "failed") {
                return(NULL)
              } else {
                y <- x$IUCN.file
                names(y) <- x$SpeciesName  
                return(y)
              }
            }))
  # browser()
  ## Step 2 - filter presence/absence based on prey community ---------
  cli_h2("Step 2 - Assemble prey dataset")
  # browser()
  
  listname.full <- checklist$SpeciesName
  names(listname.full) <- checklist$Code
  
  listcode <- occurrence.summary$Code
  names(listcode) <- occurrence.summary$SpeciesName
  # browser()
  # this.species = names(listcode)[8]
  # browser()
  species.trophic.list <- foreach(this.species = names(listcode)) %dopar% {
    cli_progress_step(this.species)
    this.code <- listcode[this.species]
    
    step2.try <- try({
      tmp <- assemble_trophic(this.species,
                              listcode,
                              file.occurrence.link = file.occurrence.link,
                              file.occurrence.rast.link = file.occurrence.rast.link,
                              file.status.rast.link = file.status.rast.link,
                              raw.dir = raw.dir,
                              param = param,
                              overwrite = overwrite.trophic,
                              fresh.occurrence = fresh.occurrence,
                              subsample.assemble =  subsample.assemble,
                              trophic.groups = trophic.groups,
                              IUCN.link = IUCN.link)
      this.trophic <- tmp$this.trophic
      this.trophic.file <- tmp$this.trophic.file
      this.metaweb <- tmp$this.metaweb
      rm(tmp); gc();
    })
    if (inherits(step2.try, "try-error")) {
      cli_process_failed()
      .write_logfile(
        out.log = paste0("Species: ", this.species, 
                         ". Status: Joining dataset failed"),
        logfile = logfile_step2,
        project.name = project.name,
        open = "a",
        silent = TRUE)
    } 
    # browser()

    # if (this.species == "Accipiter nisus") browser()
    ### Summary --------------------------------------------------------------
    summary.try <- try({
      # if(this.species == "Rana arvalis") stop()
      # predator summary
      pred.summary <-  get_trophic_summary(this.trophic)
      # prey summary
      prey.summary <-  get_prey_summary(this.trophic)
      # filtered prey
      this.metaweb.full <- 
        param@metaweb.raw %>% 
        filter(Pred_Code_new == this.code,
               Co_occur >= 1,
               Score_DD == 2,
               Stage == "adults")
      
      prey.kept <- list(listname.full[prey.summary$Prey_Code])
      names(prey.kept) <- this.species
      
      filtered.prey.names <- 
        listname.full[setdiff(this.metaweb.full$Prey_Code_new,
                              occurrence.summary$Code)]
      filtered.prey <- rep("No occurrences", length(filtered.prey.names))
      names(filtered.prey) <- filtered.prey.names
      filtered.prey.list <- list(filtered.prey)
      names(filtered.prey.list) <- this.species
    })


    if (inherits(summary.try, "try-error")) {
      .write_logfile(
        out.log = paste0("Species: ", this.species, 
                         ". Status: Summary Failed"),
        logfile = logfile_step2,
        project.name = project.name,
        open = "a",
        silent = TRUE)
      cli_process_failed()
      return(list("SpeciesName" = this.species,
                  "Code" = this.code,
                  "trophic.file" = NULL,
                  "pred.summary" = NULL,
                  "prey.summary" = NULL,
                  "filtered.prey" = NULL))
    } else {
      .write_logfile(
        out.log = paste0("Species: ", this.species, 
                         ". Status: Success",
                         ". Number of prey: ", nrow(this.metaweb)),
        logfile = logfile_step2,
        project.name = project.name,
        open = "a",
        silent = TRUE)
      cli_progress_done()
      return(list("SpeciesName" = this.species,
                  "Code" = this.code,
                  "trophic.file" = this.trophic.file,
                  "pred.summary" = pred.summary,
                  "prey.summary" = prey.summary,
                  "filtered.prey" = filtered.prey.list))
    }
  }
  # browser()

  ## Format Output -------------------------------------------------------------
  summary.predator <- rbindlist(
    lapply(species.trophic.list, function(x) x$pred.summary)
  )
  summary.prey <- rbindlist(
    lapply(species.trophic.list, function(x) x$prey.summary)
  )
  
  this.nprey <- metaweb %>% 
    filter(Co_occur >= 1,
           Score_DD == 2,
           Stage == "adults") %>% 
    group_by(Pred_Code_new) %>% 
    summarize(nprey.unfiltered = n())
  
  summary.filter <- occurrence.summary %>% 
    rename(presence_unfiltered = presence) %>% 
    right_join(summary.predator, 
               by = join_by(SpeciesName, Code)) %>% 
    mutate(presence.filtered = presence_unfiltered - presence,
           absence.filtered.inside = absence.inside.certain - absence_inside,
           absence.filtered.outside = absence.outside - absence_outside) %>% 
    left_join(this.nprey, by = c("Code" = "Pred_Code_new")) %>% 
    mutate(nprey.unfiltered = ifelse(is.na(nprey.unfiltered), 0, nprey.unfiltered),
           nprey.filtered = nprey.unfiltered - nprey) %>% 
    select(SpeciesName, Code, 
           presence_unfiltered, absence.outside, absence.inside.certain, absence.inside.uncertain,
           presence.filtered, absence.filtered.inside, absence.filtered.outside, nprey.filtered, nprey.unfiltered, nprey) %>% 
    rename(presence = presence_unfiltered) %>% 
    left_join(checklist, by = join_by(SpeciesName, Code))
  
  
  
  listname <- sapply(species.trophic.list, function(x) x$SpeciesName)
  
  file.trophic.raw.link <- 
    sapply(species.trophic.list, function(x) x$trophic.file)
  names(file.trophic.raw.link) <- listname
  
  file.trophic.link <- file.trophic.raw.link
  
  metaweb.filtered <- 
    metaweb %>% 
    filter(Pred_Code_new %in% summary.prey$Code &
             Prey_Code_new %in% summary.prey$Prey_Code)
  param@metaweb <- metaweb.filtered
  
  checklist.filtered <- 
    checklist %>% 
    filter(Code %in% occurrence.summary$Code)
  param@checklist <- checklist.filtered
  
  kept.species <- listname
  kept.prey <- sapply(species.trophic.list, function(x){
    listname.full[x$prey.summary$Prey_Code]
  })
  names(kept.prey) <- listname
  
  filtered.prey <- sapply(species.trophic.list, function(x) x$filtered.prey)
  
  if( any(colnames(occurrence.summary) == "threshold.certain")) {
    species.method <- ifelse(is.na(occurrence.summary$threshold.certain),
                             "IUCN","gbif")
  } else {
    species.method <- rep("IUCN", nrow(occurrence.summary))
  }
  species.method[!fresh.occurrence] <- "Old extraction"
  names(species.method) <- occurrence.summary$SpeciesName
  species.method <- factor(species.method, 
                           levels = c("IUCN","gbif","Old extraction"))
  output <- new('trophic_dataset')
  # browser()
  
  # fill output@summary
  output@summary@occurrence <- occurrence.summary
  output@summary@trophic <- summary.predator
  output@summary@prey <- summary.prey
  output@summary@filtered <- summary.filter
  output@summary@protected <- data.frame()
  # output@summary@patrimonial <- data.frame()
  
  output@summary.raw <- output@summary
  # fill output@files
  output@files@occurrence <- file.occurrence.link
  output@files@trophic <- file.trophic.link
  output@files@trophic.raw <- file.trophic.link
  
  # fill output@param
  output@param <- param
  
  # fill output@species
  output@species@kept <- kept.species
  output@species@filtered <- filtered.species
  output@species@kept.prey <- kept.prey
  output@species@filtered.prey <- filtered.prey
  output@species@species.method <- species.method
  # fill data.mask and project.name
  output@project.name <- project.name
  output@data.mask <- wrap(data.mask)
  saveRDS(output, file = paste0(project.name,"/trophic_dataset.rds"))
  validObject(output)
  
  ## Step 3 - Subsample & Filter output ---------------------------------------
  cli_h1("Subsampling dataset")
  # browser()
  
  cli_progress_step("Subsample dataset")
  output <- subsample(output)
  
  output
} 



### Argument Check -----------------------------------

.prepare_dataset.check.args <- function(checklist, 
                                        metaweb,
                                        use.gbif,
                                        param.gbif,
                                        param.subsampling,
                                        folder.iucn,
                                        data.mask, 
                                        project.name,
                                        nb.cpu,
                                        overwrite.occurrence,
                                        overwrite.trophic,
                                        subsample.assemble){
  
  
  #### checklist -------------------------------------------------------------
  .check_checklist(checklist)
  #### metaweb --------------------------------------------------------------- 
  .check_metaweb(metaweb)
  
  # compatibility between checklist and metaweb
  .check_metaweb_checklist(metaweb, checklist)
  
  
  #### data.mask -----------------------------
  .fun_testIfInherits(data.mask, "SpatRaster")
  
  #### project.name --------------------------
  .fun_testIfInherits(project.name, "character")
  if (!dir.exists(project.name)) dir.create(project.name, recursive = TRUE)
  
  #### param.gbif ----------------------------------------------------------------
  species.buffer <- NULL
  if (use.gbif) {
    if (missing(param.gbif)) {
      param.gbif <- set_param_gbif(use.gbif = TRUE)
    } else {
      .fun_testIfInherits(param.gbif, "param_gbif")
    }
    # get species.buffer
    species.buffer <- get_species_buffer(param.gbif@buffer.config, checklist)
    param.gbif@species.buffer <- species.buffer
  } else {
    param.gbif <- set_param_gbif(use.gbif = FALSE)
  }
  
  #### nb.cpu -------------------------------------------------------  
  .fun_testIfPosInt(nb.cpu)
  
  #### param.subsampling ----------------------------------------------------------
  if (missing(param.subsampling)) {
    param.subsampling <- set_param_subsampling()
  } else {
    .fun_testIfInherits(param.subsampling, "param_subsampling")
  }
  
  #### overwrite -------------------------------------------
  stopifnot(is.logical(overwrite.occurrence))
  stopifnot(is.logical(overwrite.trophic))
  
  #### subsample.assemble -------------------------------------------
  if (missing(subsample.assemble)) {
    subsample.assemble <- ifelse(use.gbif, FALSE, TRUE)
  } else {
    stopifnot(is.logical(subsample.assemble))
  }
  
  return(list(
    "checklist" = checklist, 
    "metaweb" = metaweb,
    "use.gbif" = use.gbif,
    "param.gbif" = param.gbif,
    "param.subsampling" = param.subsampling,
    "folder.iucn" = folder.iucn,
    "data.mask" = data.mask, 
    "project.name" = project.name,
    "nb.cpu" = nb.cpu,
    "overwrite.occurrence" = overwrite.occurrence,
    "overwrite.trophic" = overwrite.trophic,
    "subsample.assemble" = subsample.assemble))
}

