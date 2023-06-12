## --------------------------------------------------------------------------- #
# 1. extract_gbif         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name extract_gbif
##' @author Remi Patin
##'
##' @title Extract gbif occurrences for a single species
##'
##' @description \code{extract_gbif} retrieve gbif data from files with a given 
##' configuration
##'
##' @inheritParams trophic_dataset
##' @param this.species a \code{character}: species name
##' @param this.code a \code{character}: species code
##' @param param.gbif a \code{param_gbif} gbif configuration
##' @param occurrence.dir a \code{character}, folder to store occurrence data
##' @param logfile a \code{character}, file to store logs

extract_gbif <- function(this.species,
                         this.code, 
                         param.gbif,
                         occurrence.dir, 
                         project.name,
                         logfile,
                         overwrite) {
  cli_progress_step(this.species)
  this.occurrence <- paste0(occurrence.dir,this.code,".csv.gz")
  this.threshold.file <- paste0(occurrence.dir, "thresh_", this.code, ".rds")
  full.run <- !file.exists(this.occurrence) | overwrite
  # browser()
  if (full.run) {
    this.output <- load_gbif_data(species.name = this.species,
                                  folder.gbif = param.gbif@folder.gbif, 
                                  filter.atlas = param.gbif@filter.atlas)
    
    this.summary <- "failed"
    this.status <- "failed"
    this.iucn <- "failed"
    this.occurrence <- "failed"
    this.fresh <- TRUE
    
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
          locate_iucn_distribution(species.code = this.code,
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
            rename(presence = last) # was layer before ?! 
          
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
          if (length(index.uncertain) > 0) {
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
            saveRDS(this.threshold, file = this.threshold.file)
            this.uncertain.vect <- 
              this.occurrence.df[index.uncertain,] %>% 
              vect(geom = c("x","y"), crs = crs("EPSG:3035"))
            this.effort.uncertain <- extract(this.effort.rast,
                                             this.uncertain.vect)[, this.effort.layer]
            this.occurrence.df$status[index.uncertain] <- 
              ifelse(this.effort.uncertain >= this.threshold, "certain","uncertain")
          }
          this.occurrence.df$SpeciesName <- this.species
          this.occurrence.df$Code <- this.code
          fwrite(this.occurrence.df, file = this.occurrence)
          
          ### Summary --------------------------
          n.list <- get_occurrence_summary(this.occurrence.df)
          for (argi in names(n.list)) { assign(x = argi, value = n.list[[argi]]) }
          
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
  } else {
    ### Extract from previous run summary --------------------------
    this.status <- "success"
    this.reason <- "previous run"
    this.fresh <- FALSE
    this.iucn <-
      locate_iucn_distribution(species.code = this.code,
                               folder.iucn = param.gbif@folder.iucn.buffer,
                               filetype = '.tif')
    
    this.occurrence.df <- fread(this.occurrence)
    
    n.list <- get_occurrence_summary(this.occurrence.df)
    for (argi in names(n.list)) { assign(x = argi, value = n.list[[argi]]) }
    this.threshold <- 
      ifelse(file.exists(this.threshold.file),
             readRDS(this.threshold.file),
             NA)
    this.summary <- 
      data.frame("SpeciesName" = this.species,
                 "Code" = this.code,
                 "presence" = n.presences,
                 "absence" = n.absences,
                 "absence.outside" = n.absences.outside,
                 "absence.inside.certain" = n.absences.inside.certain,
                 "absence.inside.uncertain" = n.absences.inside.uncertain,
                 "threshold.certain" = NA)
  }
  
  if (this.status == "success" && 
      this.summary$presence <= param.gbif@min.gbif) {
    this.summary <- "failed"
    this.status <- "failed"
    this.reason <- "low occurrence count after rasterization"
  }
  # browser()
  
  .write_logfile(
    out.log = paste0("Species: ", this.species, 
                     ". Status: ",this.status,
                     ". Reason: ", this.reason),
    logfile = logfile,
    project.name = project.name,
    open = "a",
    silent = TRUE)
  # browser()
  
  cli_progress_done()
  
  # browser()
  
  return(list("SpeciesName" = this.species,
              "Code" = this.code,
              "status" = this.status,
              "reason" = this.reason,
              "IUCN.file" = this.iucn,
              "occurrence.file" = this.occurrence,
              "occurrence.summary" = this.summary,
              "fresh" = this.fresh))
  
}


## --------------------------------------------------------------------------- #
# 2. extract_iucn          ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name extract_iucn
##' @author Remi Patin
##'
##' @title Extract IUCN occurrences for a single species
##'
##' @description \code{extract_iucn} retrieve IUCN data from files with a given 
##' configuration
##'
##' @inheritParams trophic_dataset
##' @param this.species a \code{character}: species name
##' @param this.code a \code{character}: species code
##' @param folder.iucn a \code{character}, folder with IUCN distribution as raster
##' @param occurrence.dir a \code{character}, folder to store occurrence data
##' @param logfile a \code{character}, file to store logs

extract_iucn <- function(this.species,
                         this.code, 
                         folder.iucn,
                         occurrence.dir, 
                         project.name,
                         logfile,
                         overwrite) {
  cli_progress_step(this.species)
  # browser()
  
  this.occurrence <- paste0(occurrence.dir,this.code,".csv.gz")
  full.run <- !file.exists(this.occurrence) | overwrite
  
  # browser()
  if (full.run) {
    this.summary <- "failed"
    this.status <- "failed"
    this.iucn <- "failed"
    this.occurrence <- "failed"
    this.fresh <- TRUE
    # more than min.gbif data
    iucn.try <- try({
      this.iucn <-
        locate_iucn_distribution(species.code = this.code,
                                 folder.iucn = folder.iucn,
                                 filetype = '.tif')
      # get IUCN distribution
      this.iucn.rast <- rast(this.iucn)
      this.iucn.df <- 
        this.iucn.rast %>% 
        as.data.frame(cells = TRUE, na.rm = TRUE, xy = TRUE) %>% 
        rename(presence = layer) %>% 
        mutate(status = "certain",
               inside_iucn = (presence == 1))
    }, silent = TRUE)
    
    if (inherits(iucn.try, "try-error")) {
      this.reason <- "Could not retrieve IUCN information"
      this.iucn <- "failed"
      cli_progress_done()
    }  else {
      this.summary <- "success"
      this.status <- "success"
      this.reason <- "success"
      
      this.iucn.df$SpeciesName <- this.species
      this.iucn.df$Code <- this.code
      fwrite(this.iucn.df, file = this.occurrence)
      
      ### Summary --------------------------
      n.list <- get_occurrence_summary(this.iucn.df)
      for (argi in names(n.list)) { assign(x = argi, value = n.list[[argi]]) }
      
      this.summary <- 
        data.frame("SpeciesName" = this.species,
                   "Code" = this.code,
                   "presence" = n.presences,
                   "absence" = n.absences,
                   "absence.outside" = n.absences.outside,
                   "absence.inside.certain" = n.absences.inside.certain,
                   "absence.inside.uncertain" = n.absences.inside.uncertain)
    }
  } else {
    ### Extract from previous run summary --------------------------
    this.status <- "success"
    this.reason <- "previous run"
    this.fresh <- FALSE
    this.iucn <-
      locate_iucn_distribution(species.code = this.code,
                               folder.iucn = param.gbif@folder.iucn.buffer,
                               filetype = '.tif')
    
    this.occurrence.df <- fread(this.occurrence)
    n.list <- get_occurrence_summary(this.occurrence.df)
    for (argi in names(n.list)) { assign(x = argi, value = n.list[[argi]]) }
    this.summary <- 
      data.frame("SpeciesName" = this.species,
                 "Code" = this.code,
                 "presence" = n.presences,
                 "absence" = n.absences,
                 "absence.outside" = n.absences.outside,
                 "absence.inside.certain" = n.absences.inside.certain,
                 "absence.inside.uncertain" = n.absences.inside.uncertain)
    
  }
  .write_logfile(
    out.log = paste0("Species: ", this.species, 
                     ". Status: ",this.status,
                     ". Reason: ", this.reason),
    logfile = logfile,
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
              "occurrence.summary" = this.summary,
              "fresh" = this.fresh))
  
}

## --------------------------------------------------------------------------- #
# 3. assemble_trophic          ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name assemble_trophic
##' @author Remi Patin
##'
##' @title Extract IUCN occurrences for a single species
##'
##' @description \code{extract_iucn} retrieve IUCN data from files with a given 
##' configuration
##'
##' @inheritParams trophic_dataset
##' @param this.species a \code{character}: species name
##' @param this.code a \code{character}: species code
##' @param folder.iucn a \code{character}, folder with IUCN distribution as raster
##' @param occurrence.dir a \code{character}, folder to store occurrence data
##' @param logfile a \code{character}, file to store logs

assemble_trophic <- function(this.species, 
                             listcode,
                             file.occurrence.link,
                             raw.dir,
                             param,
                             overwrite,
                             fresh.occurrence,
                             subsample.assemble,
                             IUCN.link = IUCN.link){
  param.gbif <- param@param.gbif
  this.code <- listcode[this.species]
  ### filter metaweb ---------------------
  this.metaweb <- 
    param@metaweb.raw %>% 
    filter(Pred_Code_new == this.code,
           Prey_Code_new %in% listcode,
           Co_occur >= 1,
           Score_DD == 2,
           Stage == "adults")
  
  this.trophic.file <- paste0(raw.dir, this.code,".csv.gz")
  this.trophic.exists <- file.exists(this.trophic.file)
  # browser()
  full.run <-
    overwrite || # overwrite = TRUE
    !this.trophic.exists || # file does not exist
    fresh.occurrence[this.species] || # species occurrence have been updated
    (nrow(this.metaweb) > 0 && any(fresh.occurrence[this.metaweb$Prey_Name])) # any prey have been updated
  if (full.run) {
    this.fresh <- TRUE
    this.occurrence <- fread(file.occurrence.link[this.species]) %>% 
      filter(status == "certain") %>% 
      select(-status) 
    
    if (subsample.assemble) {
      this.occurrence <- subsample_dataset(this.occurrence, param.subsampling = param@param.subsampling)
      this.occurrence <- filter(this.occurrence, subsample)
    }
    
    if (nrow(this.metaweb) == 0) {
      ### no prey ---------------------
      this.trophic <- this.occurrence 
      fwrite(this.trophic, this.trophic.file)
    } else {
      # browser()
      
      ### with prey ---------------------
      all.prey <- this.metaweb$Prey_Name
      # this.prey <- first(this.metaweb$Prey_Name)
      if (param.gbif@use.gbif) {
        this.prey.list <- foreach(this.prey = all.prey) %do% {
          fread(file.occurrence.link[this.prey]) %>% 
            semi_join(this.occurrence, by = c("cell","x","y"))
        }
        this.prey.df <- rbindlist(this.prey.list)
        rm(this.prey.list); gc();
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
      } else { # IUCN method
        this.prey.df <- foreach(this.prey = all.prey, .combine = 'rbind') %do% {
          fread(file.occurrence.link[this.prey]) %>% 
            semi_join(this.occurrence, by = c("cell","x","y"))
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
        
      }
      this.trophic <- 
        right_join(this.occurrence, this.prey.pivot, 
                   by = c("cell","x","y")) %>% 
        mutate(subsample = TRUE)
      rm(this.prey.pivot); gc()
      rm(this.occurrence); gc()
      fwrite(this.trophic, this.trophic.file)
    }
  } else {
    this.fresh <- FALSE
    this.trophic <- fread(this.trophic.file)
  }
  list("this.trophic" = this.trophic,
       "this.trophic.file" = this.trophic.file,
       "this.metaweb" = this.metaweb,
       "this.fresh" = this.fresh)
}