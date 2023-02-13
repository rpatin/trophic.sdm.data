
### prepare_dataset_gbif -----------------------------------
##'
##' @rdname trophic_dataset
##' @export
##' @inheritParams .register_cluster
##' @importFrom terra focal rast vect extract crs
##' @importFrom dplyr rename filter semi_join select mutate first summarize
##' @importFrom tidyr pivot_wider
##' @importFrom data.table fwrite fread

prepare_dataset_gbif <- function(checklist, 
                                 metaweb,
                                 folder.gbif,
                                 folder.iucn.buffer,
                                 sampling.effort,
                                 data.mask,
                                 project.name,
                                 min.gbif,
                                 buffer.iucn,
                                 quantile.absence.certain,
                                 prop.prey.certain,
                                 uncertain.value,
                                 nb.cpu,
                                 ...){
  
  cli_h1("Prepare trophic dataset with gbif data")
  cli_progress_step("Argument check")
  args <- .prepare_dataset_gbif.check.args(
    checklist = checklist, 
    metaweb = metaweb ,
    folder.gbif = folder.gbif,
    folder.iucn.buffer = folder.iucn.buffer,
    sampling.effort = sampling.effort,
    data.mask = data.mask,
    project.name = project.name,
    min.gbif = min.gbif,
    buffer.iucn = buffer.iucn,
    quantile.absence.certain = quantile.absence.certain,
    prop.prey.certain = prop.prey.certain,
    uncertain.value = uncertain.value,
    nb.cpu = nb.cpu,
    ...)
  for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
  
  ## initialize folder -----------------------------------------------------
  folder.names <- set_folder(project.name)
  for (argi in names(folder.names)) { assign(x = argi, value = folder.names[[argi]]) }
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)
  
  ## initialize variables -----------------------------------------------------
  param = list("param.filter" = param.filter,
               "min.gbif" = min.gbif,
               "buffer.iucn" = buffer.iucn,
               "species.buffer" = species.buffer,
               "quantile.absence.certain" = quantile.absence.certain,
               "species.quantile" = species.quantile,
               "prop.prey.certain" = prop.prey.certain,
               "uncertain.value" = uncertain.value)
  
  listcode <- checklist$Code
  names(listcode) <- checklist$SpeciesName
  # init log -------------------------------------------------
  logfile_step1 <- "Step1_speciesOccurrence.log"
  link_logfile_step1 <- paste0(project.name,"/log/",logfile_step1)
  if (file.exists(link_logfile_step1)) file.remove(link_logfile_step1)
  logfile_step2 <- "Step2_assemblePreyData.log"
  link_logfile_step2 <- paste0(project.name,"/log/",logfile_step2)
  if (file.exists(link_logfile_step2)) file.remove(link_logfile_step2)
  
  # browser()
  # this.species = names(listcode)[1]
  # this.species = names(listcode)[456]
  # this.species = names(listcode)[510]
  
  cli_progress_done()
  ## Step 1 - Buffer IUCN ranges + Generate Species Occurrence dataset ---------
  cli_h2("Step 1 - Individual species occurrence dataset")
  species.occurrence <- foreach(this.species = names(listcode)) %dopar% {
    cli_progress_step(this.species)
    this.code <- listcode[this.species]
    this.output <- load_gbif_data(species.name = this.species,
                                  folder.gbif = folder.gbif)
    this.summary <- "failed"
    this.status <- "failed"
    this.iucn <- "failed"
    this.occurrence <- "failed"
    if (!inherits(this.output,"data.frame")) {
      # no gbif data
      this.reason <- this.output
    } else if (nrow(this.output) < min.gbif) {
      # less than min.gbif data
      this.reason <- paste0("less than ", min.gbif, " occurrence")
    } else if (first(this.output$distance_to_iucn) != "No distribution found" &&
               all(this.output$distance_to_iucn > 
                   buffer.iucn[[species.buffer[[this.species]]]]*1000)) {
      this.reason <- paste0("no occurrences within IUCN range")
    } else {
      # more than min.gbif data
      this.occurrence <- paste0(occurrence.dir, this.code, ".csv")
      iucn.try <- try({
        this.iucn <-
          locate_iucn_distribution(species.code = listcode[this.species],
                                   folder.iucn = folder.iucn.buffer,
                                   filetype = '.tif')
        
        if (first(this.output$distance_to_iucn) == "No distribution found" ||
            is.null(this.iucn)) {
          this.output <- 
            mutate(this.output, inside_iucn = TRUE)
          this.iucn <- "none"
        } else {
          this.buffer <- buffer.iucn[[species.buffer[[this.species]]]]*1000
          
          # buffer presences
          this.output <- 
            mutate(this.output,
                   distance_to_iucn = as.numeric(distance_to_iucn),
                   inside_iucn = distance_to_iucn <= this.buffer)
          
          # get IUCN distribution
          this.iucn.rast <- rast(this.iucn)
        }
      })
      if (inherits(iucn.try, "try-error")) {
        this.reason <- "Could not retrieve IUCN information"
        this.iucn <- "failed"
      } else {
        
        certain.try <- try({
          ### Rule 1 filter out gbif data outside IUCN ------------------
          # 
          # transform to SpatVector
          # project to ETRS1989
          this.output.vect <- 
            this.output %>% 
            filter(inside_iucn) %>%  # Rule 1
            vect(geom = c("X","Y"), crs = crs("EPSG:4326"))
          this.output.etrs <- project(this.output.vect, crs("EPSG:3035"))
          
          # rasterize occurrence to 0/1
          this.occurrence.rast <- 
            this.output.etrs %>% 
            rasterize(data.mask, background = 0) %>% 
            mask(mask = data.mask, maskvalue = NA)
          
          # transform into data.frame
          this.occurrence.df <- 
            this.occurrence.rast %>% 
            as.data.frame(cells = TRUE, na.rm = TRUE, xy = TRUE) %>% 
            rename(presence = MaskNew5Km)
          
          # add inside/outside IUCN range
          if (this.iucn == "none") {
            this.occurrence.df$inside_iucn <- TRUE
          } else {
            this.iucn.df <- 
              this.iucn.rast %>% 
              as.data.frame(cells = TRUE, na.rm = TRUE, xy = TRUE) %>% 
              rename(inside_iucn = layer) %>% 
              mutate(inside_iucn = (inside_iucn == 1))
            this.occurrence.df <- 
              left_join(this.occurrence.df, this.iucn.df,
                        by = c("cell","x","y"))
          }
          
          # transform absences into certain/uncertain
          this.occurrence.df$status <- NA
          
          ### Rule 2: presence inside IUCN are certain ------------------------
          this.occurrence.df$status[
            which(this.occurrence.df$presence == 1) 
          ] <- "certain"
          
          ### Rule 3: absences outside IUCN are certain ----------------------
          this.occurrence.df$status[
            which(this.occurrence.df$presence == 0 &
                    !this.occurrence.df$inside_iucn) 
          ] <- "certain"
          
          ### Rule 4: absences within IUCN needs a check against sampling effort -------
          index.uncertain <- which(this.occurrence.df$presence == 0 &
                                     this.occurrence.df$inside_iucn) 
          if(length(index.uncertain) > 0){
            this.effort.layer <- sampling.effort@species.layer[[this.species]]
            this.effort.rast <- rast(sampling.effort@data)[[this.effort.layer]]
            this.quantile <-
              quantile.absence.certain[[species.quantile[[this.species]]]]
            # extract threshold to caracterize absence certainty
            # i.e. quantile of sampling effort in presence cell
            this.presence.vect <- 
              this.occurrence.df %>% 
              filter(presence == 1) %>% 
              vect(geom = c("x","y"), crs = crs("EPSG:3035"))
            this.effort <- extract(this.effort.rast,
                                   this.presence.vect)[, this.effort.layer]
            this.threshold <- quantile(this.effort, probs = this.quantile)
            
            this.uncertain.vect <- 
              this.occurrence.df[index.uncertain,] %>% 
              vect(geom = c("x","y"), crs = crs("EPSG:3035"))
            this.effort.uncertain <- extract(this.effort.rast,
                                             this.uncertain.vect)[, this.effort.layer]
            this.occurrence.df$status[index.uncertain] <- 
              ifelse(this.effort.uncertain >= this.threshold, "certain","uncertain")
          }
          this.occurrence <- paste0(occurrence.dir,this.code,".csv.gz")
          this.occurrence.df$SpeciesName <- this.species
          this.occurrence.df$Code <- this.code
          fwrite(this.occurrence.df, file = this.occurrence)
          
          ### Summary --------------------------
          n.presences <- length(which(this.occurrence.df$presence == 1))
          n.absences <- length(which(this.occurrence.df$presence == 0))
          n.absences.outside <- 
            length(which(this.occurrence.df$presence == 0 &
                           !this.occurrence.df$inside_iucn))
          n.absences.inside.certain <-
            length(which(this.occurrence.df$presence == 0 &
                           this.occurrence.df$inside_iucn & 
                           this.occurrence.df$status == "certain"))
          n.absences.inside.uncertain <-
            length(which(this.occurrence.df$presence == 0 &
                           this.occurrence.df$inside_iucn & 
                           this.occurrence.df$status == "uncertain"))
          this.summary <- 
            data.frame("SpeciesName" = this.species,
                       "Code" = this.code,
                       "presence" = n.presences,
                       "absence" = n.absences,
                       "absence.outside" = n.absences.outside,
                       "absence.inside.certain" = n.absences.inside.certain,
                       "absence.inside.uncertain" = n.absences.inside.uncertain)
        })
        
        if (inherits(certain.try, "try-error")){
          this.reason <- "Error in rasterization/occurrence assessment"
        } else {
          this.status <- "success"
          this.reason <- "success"
        }
      }
    }
    .write_logfile(
      out.log = paste0("Species: ", this.species, 
                       ". Status: ",this.status,
                       ". Reason: ", this.reason),
      logfile = logfile_step1,
      project.name = project.name,
      open = "a",
      silent = TRUE)
    cli_progress_done()
    return(list("SpeciesName" = this.species,
                "Code" = this.code,
                "status" = this.status,
                "reason" = this.reason,
                "IUCN.file" = this.iucn,
                "occurrence.file" = this.occurrence,
                "occurrence.summary" = this.summary))
  } # end foreach Step 1
  
  occurrence.summary <- lapply(species.occurrence, function(x){
    if (inherits(x$occurrence.summary, "data.frame")) {
      return(x$occurrence.summary)
    } else {
      return(NULL)
    }
  }) %>% 
    rbindlist()
  
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

  ## Step 2 - filter presence/absence based on prey community ---------
  cli_h2("Step 2 - Assemble prey dataset")
  
  listname.full <- checklist$SpeciesName
  names(listname.full) <- checklist$Code
  
  listcode <- occurrence.summary$Code
  names(listcode) <- occurrence.summary$SpeciesName

  # this.species = names(listcode)[8]
  # browser()
  species.trophic.list <- foreach(this.species = names(listcode)) %dopar% {
    cli_progress_step(this.species)
    this.code <- listcode[this.species]
    
    step2.try <- try({
      ### filter metaweb ---------------------
      this.metaweb <- 
        metaweb %>% 
        filter(Pred_Code_new == this.code,
               Prey_Code_new %in% listcode,
               Co_occur >= 1,
               Score_DD == 2,
               Stage == "adults")
      
      this.occurrence <- fread(file.occurrence.link[this.species])
      this.trophic.file <- paste0(raw.dir, this.code,".csv.gz")
      
      if (nrow(this.metaweb) == 0) {
        ### no prey ---------------------
        this.trophic <- 
          this.occurrence %>% 
          filter(status == "certain") %>% 
          select(-status)
        fwrite(this.trophic, this.trophic.file)
      } else {
        ### with prey ---------------------
        all.prey <- this.metaweb$Prey_Name
        # this.prey <- first(this.metaweb$Prey_Name)
        this.prey.df <- foreach(this.prey = all.prey, .combine = 'rbind') %do% {
          fread(file.occurrence.link[this.prey])
        }
        this.prey.certain <- 
          this.prey.df %>% 
          group_by(cell,x,y) %>% 
          summarize(percent_certain = length(which(status == "certain"))/n(),
                    .groups = 'drop') %>% 
          filter(percent_certain >= prop.prey.certain)
        
        this.prey.pivot <- 
          this.prey.df %>% 
          semi_join(this.prey.certain, by = c("cell","x","y")) %>% 
          mutate(presence = ifelse(status == "certain", presence, uncertain.value)) %>% 
          select(-SpeciesName, -inside_iucn, -status) %>% 
          pivot_wider(names_from = Code, values_from = presence)
        
        rm(this.prey.df); gc()
        
        this.trophic <- 
          right_join(this.occurrence, this.prey.pivot, 
                     by = c("cell","x","y")) %>% 
          filter(status == "certain") %>% 
          select(-status)
        fwrite(this.trophic, this.trophic.file)
      }
    })
    if (inherits(step2.try, "try-error")){
      cli_process_failed()
      .write_logfile(
        out.log = paste0("Species: ", this.species, 
                         ". Status: Failed"),
        logfile = logfile_step2,
        project.name = project.name,
        open = "a",
        silent = TRUE)
    } 
    ### Summary --------------------------------------------------------------
    summary.try <- try({
      # predator summary
      pred.summary <-  get_predator_summary(this.trophic)
      # prey summary
      prey.summary <-  get_prey_summary(this.trophic)
      # filtered prey
      this.metaweb.full <- 
        metaweb %>% 
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
      return(NULL) 
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
  
  ## Format Output -------------------------------------------------------------
  # browser()
  summary.predator <- rbindlist(
    lapply(species.trophic.list, function(x) x$pred.summary)
  )
  summary.prey <- rbindlist(
    lapply(species.trophic.list, function(x) x$prey.summary)
  )
  listname <- sapply(species.trophic.list, function(x) x$SpeciesName)
  
  file.trophic.raw.link <- 
    sapply(species.trophic.list, function(x) x$trophic.file)
  names(file.trophic.raw.link) <- listname
  
  file.trophic.link <- file.trophic.raw.link
  
  metaweb.filtered <- 
    metaweb %>% 
    filter(Pred_Code_new %in% summary.prey$Code &
             Prey_Code_new %in% summary.prey$Prey_Code)
  
  checklist.filtered <- 
    checklist %>% 
    filter(Code %in% occurrence.summary$Code)
  
  kept.species <- listname
  kept.prey <- sapply(species.trophic.list, function(x){
    listname.full[x$prey.summary$Prey_Code]
  })
  names(kept.prey) <- listname
  
  filtered.prey <- sapply(species.trophic.list, function(x) x$filtered.prey)
  
  output <- new('trophic_dataset')
  output@summary.occurrence <- occurrence.summary
  output@summary.predator <- summary.predator
  output@summary.prey <- summary.prey
  output@file.occurrence.link <- file.occurrence.link
  output@file.trophic.link <- file.trophic.link
  output@metaweb.filtered <- metaweb.filtered
  output@checklist.filtered <- checklist.filtered
  output@kept.species <- kept.species
  output@filtered.species <- filtered.species
  output@kept.prey <- kept.prey
  output@filtered.prey <- filtered.prey
  output@param.raw <- 
    list(
      "summary.occurrence.raw" = occurrence.summary,
      "summary.predator.raw" = summary.predator,
      "summary.prey.raw" = summary.prey,
      "file.trophic.raw.link" = file.trophic.link,
      "metaweb" = metaweb,
      "checklist" = checklist,
      "kept.species.raw" = kept.species,
      "filtered.species.raw" = filtered.species,
      "kept.prey.raw" = kept.prey,
      "filtered.prey.raw" = filtered.prey 
    )
  output@param <- param
  output@datatype <- 'gbif'
  output@project.name <- project.name
  output@sampling.effort <- sampling.effort
  output@data.mask <- data.mask
  
  ## Subsample & Filter output ------------------------------------------------
  cli_h1("Filtering & Subsampling dataset")
  cli_progress_step("Filter Species")
  output <- filter_species(output,
                           min.presence = param.filter$min.presence,
                           min.absence = param.filter$min.absence)
  
  cli_progress_step("Subsample dataset")
  output <- subsample(output,
                      subsample.min.prevalence = param.filter$subsample.min.prevalence,
                      subsample.max.absence = param.filter$subsample.max.absence)
  
  cli_progress_step("Filter Prey")
  output <- filter_prey(output,
                        min.prevalence.prey = param.filter$min.prevalence.prey,
                        min.absence.prey = param.filter$min.absence.prey)
  output
} 



### Argument Check -----------------------------------

.prepare_dataset_gbif.check.args <- function(checklist, 
                                             metaweb,
                                             folder.gbif,
                                             folder.iucn.buffer,
                                             sampling.effort,
                                             data.mask,
                                             project.name,
                                             min.gbif,
                                             buffer.iucn,
                                             quantile.absence.certain,
                                             prop.prey.certain,
                                             uncertain.value,
                                             nb.cpu,
                                             ...){
  
  
  #### checklist -------------------------------------------------------------
  .check_checklist(checklist)
  #### metaweb --------------------------------------------------------------- 
  .check_metaweb(metaweb)
  
  # compatibility between checklist and metaweb
  .check_metaweb_checklist(metaweb, checklist)

  #### Folder, mask and sampling effort ---------------------------------------
  #### folder.gbif
  .fun_testIfInherits(folder.gbif, "character")
  .fun_testIfDirExists(folder.gbif)
  #### folder.iucn.buffer
  .fun_testIfInherits(folder.iucn.buffer, "character")
  .fun_testIfDirExists(folder.iucn.buffer)
  #### sampling.effort
  .fun_testIfInherits(sampling.effort,"sampling_effort")
  #### data.mask
  .fun_testIfInherits(data.mask, "SpatRaster")
  #### project.name
  .fun_testIfInherits(project.name, "character")
  if (!dir.exists(project.name)) dir.create(project.name, recursive = TRUE)
  
  #### min.gbif
  if (missing(min.gbif)) {
    min.gbif <- 25
    cli_alert_info("Set default min.gbif = 25")
  } else {
    .fun_testIfPosInt(min.gbif)
  }
  
  #### buffer.iucn -----------------------------------------------------------
  if (missing(buffer.iucn)) {
    stop("Please provide argument buffer.iucn, a named list with the distance used to buffer IUCN range, by taxonomic group")
  } else {
    .fun_testIfPosNum(unlist(buffer.iucn))
    buffer.name <- names(buffer.iucn)
    check.taxa <- sapply(buffer.name, function(x) check_taxa(x, checklist))
    if ( !all(check.taxa) ) {
      for (this.taxa in which(!check.taxa)) {
        cli_alert_danger("taxa {buffer.name[this.taxa]} not found in the \\
                         provided checklist")
      }
      stop(paste0("Some taxa given in buffer.iucn were not found."))
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
      cli_alert_danger("the following species were assigned no buffer.iucn \\
                       values: {names(species.buffer)[species.unassigned]}")
      stop("Please review buffer.iucn to have all species covered")
    }
  }
  
  #### quantile.absence.certain ---------------------------------------------
  
  # missing case
  if (missing(quantile.absence.certain)) {
    cli_alert_info("quantile.absence.certain set to default 10% for all taxonomic group")
    quantile.absence.certain <- as.list(rep(0.1,length(unique(checklist$Class))))
    names(quantile.absence.certain) <- unique(checklist$Class)
  } 
  
  # 1-length numeric
  if (is.numeric(quantile.absence.certain)) {
    if ( length(quantile.absence.certain) > 1) {
      stop("quantile.absence.certain must have a length of 1")
    }
    if ( quantile.absence.certain < 0 ||
         quantile.absence.certain > 1) {
      stop("quantile.absence.certain must be included between 0 and 1")
    }
    cli_alert_info("quantile.absence.certain set to {quantile.absence.certain}\\
                   for all taxonomic groups.")
    quantile.absence.certain <- as.list(rep(quantile.absence.certain,
                                            length(unique(checklist$Class))))
    names(quantile.absence.certain) <- unique(checklist$Class)
  }
  
  # now we have a list
  .fun_testIf01(unlist(quantile.absence.certain))
  quantile.name <- names(quantile.absence.certain)
  check.taxa <- sapply(quantile.name, function(x) check_taxa(x, checklist))
  if ( !all(check.taxa) ) {
    for (this.taxa in which(!check.taxa)) {
      cli_alert_danger("taxa {quantile.name[this.taxa]} not found in the \\
                         provided checklist")
    }
    stop(paste0("Some taxa given in quantile.absence.certain were not found."))
  }
  species.quantile <- lapply(seq_len(nrow(checklist)), function(x){
    checklist.sub <- checklist[x,]
    for (level_to_check in c("SpeciesName",
                             "Family",
                             "Order",
                             "Class")) {
      this.check <- sapply(quantile.name,  function(x) x == checklist.sub[ , level_to_check])
      if (any(this.check)) {
        return(quantile.name[which(this.check)]) 
      }
    }
    "none"
  })
  names(species.quantile) <- checklist$SpeciesName
  species.unassigned <- which(unlist(species.buffer) == "none")
  if (length(species.unassigned) > 0) {
    cli_alert_danger("the following species were assigned no \\
                     quantile.absence.certain values: \\
                     {names(species.quantile)[species.unassigned]}")
    stop("Please review quantile.absence.certain to have all species covered")
  }
  
  # browser()
  ####  prop.prey.certain -----------------------------------------------
  if (missing(prop.prey.certain)) {
    cli_alert_info("Set prop.prey.certain to default 80%")
    prop.prey.certain <- 0.8
  }
  .fun_testIf01(prop.prey.certain)
  
  #### uncertain.value -------------------------------------------------------
  if (missing(uncertain.value)) {
    cli_alert_info("Fixed uncertain.value to default 0")
    uncertain.value <- 0
  }
  if ( length(uncertain.value) != 1 ||
       !(uncertain.value %in% c(0,1))) {
    stop("uncertain.value must be 0 or 1")
  }
  #### nb.cpu -------------------------------------------------------  
  .fun_testIfPosInt(nb.cpu)
  #### param.filter (...) ----------------------------------------------------------
  param.filter <- .filter_dataset.check.args(type = "all", ...)
  
  return(list(
    "checklist" = checklist, 
    "metaweb" = metaweb ,
    "folder.gbif" = folder.gbif,
    "folder.iucn.buffer" = folder.iucn.buffer,
    "sampling.effort" = sampling.effort,
    "data.mask" = data.mask,
    "project.name" = project.name,
    "min.gbif" = min.gbif,
    "buffer.iucn" = buffer.iucn,
    "species.buffer" = species.buffer,
    "quantile.absence.certain" = quantile.absence.certain,
    "species.quantile" = species.quantile,
    "prop.prey.certain" = prop.prey.certain,
    "uncertain.value" = uncertain.value,
    "nb.cpu" = nb.cpu,
    "param.filter" = param.filter
  ))
}