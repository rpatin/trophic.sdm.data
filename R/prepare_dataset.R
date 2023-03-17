
### prepare_dataset -----------------------------------
##'
##' @rdname trophic_dataset
##' @export
##' @inheritParams .register_cluster
##' @importFrom terra focal rast vect extract crs
##' @importFrom dplyr rename filter semi_join select mutate first summarize
##' @importFrom tidyr pivot_wider
##' @importFrom data.table fwrite fread

prepare_dataset <- function(checklist, 
                            metaweb,
                            use.gbif = TRUE,
                            param.gbif,
                            param.subsampling,
                            folder.iucn,
                            data.mask,
                            project.name,
                            nb.cpu,
                            ...){
  
  cli_h1("Prepare trophic dataset data")
  cli_progress_step("Argument check")
  args <- .prepare_dataset.check.args(
    checklist = checklist, 
    metaweb = metaweb,
    use.gbif = use.gbif,
    param.gbif = param.gbif,
    param.subsampling = param.subsampling,
    folder.iucn = folder.iucn,
    data.mask = data.mask, 
    project.name = project.name,
    nb.cpu = nb.cpu)
  for (argi in names(args)) { assign(x = argi, value = args[[argi]]) }
  
  ## initialize folder -----------------------------------------------------
  folder.names <- set_folder(project.name)
  for (argi in names(folder.names)) { assign(x = argi, value = folder.names[[argi]]) }
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)
  
  ## initialize variables -----------------------------------------------------
  param = new("param_trophic")
  param@checklist.raw <- checklist
  param@metaweb.raw <- metaweb
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
  
  # browser()
  this.species = names(listcode)[1]
  # this.species = names(listcode)[456]
  # this.species = names(listcode)[510]
  
  cli_progress_done()
  ## Step 1 - Buffer IUCN ranges + Generate Species Occurrence dataset ---------
  cli_h2("Step 1 - Individual species occurrence dataset")
  species.occurrence <- foreach(this.species = names(listcode)) %dopar% {
    cli_progress_step(this.species)
    this.code <- listcode[this.species]
    this.output <- load_gbif_data(species.name = this.species,
                                  folder.gbif = param.gbif@folder.gbif, 
                                  filter.atlas = param.gbif@filter.atlas)
    this.summary <- "failed"
    this.status <- "failed"
    this.iucn <- "failed"
    this.occurrence <- "failed"
    if (!inherits(this.output,"data.frame")) {
      # no gbif data
      this.reason <- this.output
    } else if (nrow(this.output) < param.gbif@min.gbif) {
      # less than min.gbif data
      this.reason <- paste0("less than ", param.gbif@min.gbif, " occurrence")
    } else if (first(this.output$distance_to_iucn) != "No distribution found" &&
               all(this.output$distance_to_iucn > 
                   param.gbif@buffer.config[[
                     param.gbif@species.buffer[[this.species]]]]*1000
               )) {
      this.reason <- paste0("no occurrences within IUCN range")
    } else {
      # more than min.gbif data
      this.occurrence <- paste0(occurrence.dir, this.code, ".csv")
      iucn.try <- try({
        this.iucn <-
          locate_iucn_distribution(species.code = listcode[this.species],
                                   folder.iucn = param.gbif@folder.iucn.buffer,
                                   filetype = '.tif')
        
        if (first(this.output$distance_to_iucn) == "No distribution found" ||
            is.null(this.iucn)) {
          this.output <- 
            mutate(this.output, inside_iucn = TRUE)
          this.iucn <- "none"
        } else {
          this.buffer <- 
            param.gbif@buffer.config[[
              param.gbif@species.buffer[[this.species]]]]*1000
          
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
            rename(presence = mask.grid)
          
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
              param.gbif@quantile.absence.certain
            # extract threshold to caracterize absence certainty
            # i.e. quantile of sampling effort in presence cell
            this.presence.vect <- 
              this.occurrence.df %>% 
              filter(presence == 1) %>% 
              vect(geom = c("x","y"), crs = crs("EPSG:3035"))
            this.effort <- extract(this.effort.rast,
                                   this.presence.vect)[, this.effort.layer]
            this.threshold <- quantile(this.effort, probs = this.quantile, type = 3)
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
                       "absence.inside.uncertain" = n.absences.inside.uncertain,
                       "threshold.certain" = this.threshold)
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
          select(-status) %>% 
          mutate(subsample = TRUE)
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
          filter(percent_certain >= param.gbif@prop.prey.certain)
        
        this.prey.pivot <- 
          this.prey.df %>% 
          semi_join(this.prey.certain, by = c("cell","x","y")) %>% 
          mutate(presence = ifelse(status == "certain", presence,
                                   param.gbif@uncertain.value)) %>% 
          select(-SpeciesName, -inside_iucn, -status) %>% 
          pivot_wider(names_from = Code, values_from = presence)
        
        rm(this.prey.df); gc()
        
        this.trophic <- 
          right_join(this.occurrence, this.prey.pivot, 
                     by = c("cell","x","y")) %>% 
          filter(status == "certain") %>% 
          select(-status) %>% 
          mutate(subsample = TRUE)
        fwrite(this.trophic, this.trophic.file)
      }
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
    # if (this.species == "Accipiter nisus") browser()
    
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
  output <- new('trophic_dataset')
  
  # fill output@summary
  output@summary@occurrence <- occurrence.summary
  output@summary@trophic <- summary.predator
  output@summary@prey <- summary.prey
  output@summary@filtered <- summary.filter
  output@summary@protected <- data.frame()
  output@summary@patrimonial <- data.frame()
  
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
  
  # fill data.mask and project.name
  output@project.name <- project.name
  output@data.mask <- wrap(data.mask)
  saveRDS(output, file = paste0(project.name,"/",project.name,".trophic_dataset.rds"))
  
  ## Subsample & Filter output ------------------------------------------------
  cli_h1("Subsampling dataset")
  
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
                                        nb.cpu){
  
  
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
  
  return(list(
    "checklist" = checklist, 
    "metaweb" = metaweb,
    "use.gbif" = use.gbif,
    "param.gbif" = param.gbif,
    "param.subsampling" = param.subsampling,
    "folder.iucn" = folder.iucn,
    "data.mask" = data.mask, 
    "project.name" = project.name,
    "nb.cpu" = nb.cpu))
}

