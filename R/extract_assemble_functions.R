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
##' @param overwrite a \code{boolean}, whether occurrence dataset should be 
##' overwritten.
##' @param occurrence.rast.dir a \code{character}, folder to store occurrence 
##' raster files.
##' @param status.rast.dir a \code{character}, folder to store data status 
##' raster files.

extract_gbif <- function(this.species,
                         this.code, 
                         param.gbif,
                         data.mask,
                         occurrence.dir, 
                         occurrence.rast.dir, 
                         status.rast.dir, 
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
      this.occurrence <- paste0(occurrence.dir, this.code, ".csv.gz")
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
            this.effort.layer <- param.gbif@sampling.effort@species.layer[[this.species]]
            this.effort.rast <- rast(param.gbif@sampling.effort@data)[[this.effort.layer]]
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
          # browser()
          fwrite(this.occurrence.df, file = this.occurrence, showProgress = FALSE)
          
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
    
    this.occurrence.df <- fread(this.occurrence, showProgress = FALSE)
    
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
  
  
  # browser()
  raster.files <- occurrences_as_raster(occurrence.file = this.occurrence, 
                                        data.mask = data.mask, 
                                        occurrence.rast.dir = occurrence.rast.dir,
                                        status.rast.dir = status.rast.dir,
                                        species = this.code,
                                        full.run = full.run)
  
  cli_progress_done()
  return(list("SpeciesName" = this.species,
              "Code" = this.code,
              "status" = this.status,
              "reason" = this.reason,
              "IUCN.file" = this.iucn,
              "occurrence.file" = this.occurrence,
              "occurrence.rast.file" = raster.files$occ,
              "status.rast.file" = raster.files$status,
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
##' @inheritParams extract_gbif
##' @param folder.iucn a \code{character}, folder with IUCN distribution as raster
##' @param occurrence.rast.dir a \code{character}, folder to store occurrence 
##' raster files.
##' @param status.rast.dir a \code{character}, folder to store data status 
##' raster files.


extract_iucn <- function(this.species,
                         this.code, 
                         data.mask = data.mask,
                         folder.iucn,
                         occurrence.dir, 
                         occurrence.rast.dir, 
                         status.rast.dir, 
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
      this.occurrence <- "failed"
      this.reason <- "Could not retrieve IUCN information"
      this.iucn <- "failed"
      cli_progress_done()
    }  else {
      this.summary <- "success"
      this.status <- "success"
      this.reason <- "success"
      
      this.iucn.df$SpeciesName <- this.species
      this.iucn.df$Code <- this.code
      fwrite(this.iucn.df, file = this.occurrence, showProgress = FALSE)
      
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
                               folder.iucn = folder.iucn,
                               filetype = '.tif')
    
    this.occurrence.df <- fread(this.occurrence, showProgress = FALSE)
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
  
  # browser()
  raster.files <- occurrences_as_raster(occurrence.file = this.occurrence, 
                                        data.mask = data.mask, 
                                        occurrence.rast.dir = occurrence.rast.dir,
                                        status.rast.dir = status.rast.dir,
                                        species = this.code,
                                        full.run = full.run)
  

  cli_progress_done()
  return(list("SpeciesName" = this.species,
              "Code" = this.code,
              "status" = this.status,
              "reason" = this.reason,
              "IUCN.file" = this.iucn,
              "occurrence.file" = this.occurrence,
              "occurrence.rast.file" = raster.files$occ,
              "status.rast.file" = raster.files$status,
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
##' @param this.species a \code{character}: current species name
##' @param listcode a named \code{character} vector: all species code
##' @param file.occurrence.link a \code{character} vector, links to
##'  occurrence files
##' @param file.occurrence.rast.link a \code{character} vector, links 
##' to occurrence files as raster
##' @param file.status.rast.link a \code{character} vector, links 
##' to data status files as raster (certain or uncertain data)
##' @param raw.dir a \code{character} vector, folder in which to store raw 
##' (not subsampled) trophic dataset
##' @param param a \code{\link{param_trophic}} object with all parameters
##' @param overwrite a \code{boolean}. When TRUE, all trophic dataset are 
##' recalculated. When FALSE, only trophic dataset for which prey or predator data
##' were refreshed are recalculated.
##' @param fresh.occurrence a named \code{boolean} vector, describing which 
##' species occurrence were updated.
##' @param subsample.assemble a \code{boolean}, whether some subsampling 
##' occurs before assembling the trophic dataset.
##' @param trophic.groups a \code{data.frame}, describing the trophic groups
##' @param IUCN.link a named \code{character} vector, with links to species
##' IUCN data.
##' @importFrom terra app
assemble_trophic <- function(this.species, 
                             listcode,
                             file.occurrence.link,
                             file.occurrence.rast.link,
                             file.status.rast.link,
                             raw.dir,
                             param,
                             overwrite,
                             fresh.occurrence,
                             subsample.assemble,
                             trophic.groups,
                             IUCN.link){
  param.gbif <- param@param.gbif
  this.code <- listcode[this.species]
  # browser()
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
    this.occurrence <- fread(file.occurrence.link[this.species], showProgress = FALSE) %>% 
      filter(status == "certain") %>% 
      select(-status) 
    
    # if (subsample.assemble) {
    #   this.occurrence <- subsample_dataset(this.occurrence, param.subsampling = param@param.subsampling)
    #   this.occurrence <- filter(this.occurrence, subsample)
    # }
    
    if (nrow(this.metaweb) == 0) {
      ### no prey ---------------------
      this.trophic <- this.occurrence 
      fwrite(this.trophic, this.trophic.file, showProgress = FALSE)
    } else {
      # browser()
      this.occurrence.rast <- rast(file.occurrence.rast.link[this.species])
      this.status.rast <- rast(file.status.rast.link[this.species])
      this.occurrence.certain <- mask(this.occurrence.rast, this.status.rast, maskvalues = 0)
      ### with prey ---------------------
      all.prey <- this.metaweb$Prey_Name
      all.code <- listcode[all.prey]
      prey.groups <- 
        trophic.groups %>% 
        filter(Code %in% all.code) %>% 
        select(Code, sppname, group)
      # this.prey <- first(this.metaweb$Prey_Name)
      # browser()
      # Step 1: calculate mask with certain prey
      this.status.rast <- foreach(this.prey = all.prey, .combine = 'c') %do% {
        rast(file.status.rast.link[this.prey])
      }
      # browser()
      this.status <- app(this.status.rast, mean)
      this.status.kept <- this.status >= param.gbif@prop.prey.certain
      this.prey.mask <- mask(this.status.kept, 
                             this.occurrence.certain)
      
      # browser()
      this.covariates <- foreach(this.group = unique(prey.groups$group), .combine = 'c') %do% {
        this.label <- paste0("group_",this.group)
        this.prey.group <- filter(prey.groups, group %in% this.group)
        this.group.rast <- foreach(this.prey = unique(this.prey.group$sppname), .combine = 'c') %do% {
          rast(file.occurrence.rast.link[this.prey]) %>% 
            mask(this.prey.mask)
        }
        this.sum.rast <- sum(this.group.rast,
                             wopt = list(names = this.label))
        this.sum.rast
      } 
      this.covariates.df <- as.data.frame(this.covariates, cells = TRUE, xy = TRUE)
      this.trophic <- 
        this.occurrence %>% 
        right_join(this.covariates.df, by = c("cell","x","y"))  %>% 
        mutate(subsample = TRUE)
      rm(this.covariates.df, this.occurrence);gc();
      fwrite(this.trophic, this.trophic.file, showProgress = FALSE)
    }
  } else {
    this.fresh <- FALSE
    this.trophic <- fread(this.trophic.file, showProgress = FALSE)
  }
  list("this.trophic" = this.trophic,
       "this.trophic.file" = this.trophic.file,
       "this.metaweb" = this.metaweb,
       "this.fresh" = this.fresh)
}