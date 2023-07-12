## Load gbif data ----------------------------
##' @name load_gbif_data
##' 
##' @title Load gbif data
##' 
##' @description \code{load_gbif_data} find data for a given species 
##' within \code{folder.gbif}. Data is then read as a data.frame
##' 
##' @inheritParams gbif_outsider
##' @param species.name a \code{character} corresponding to the species name
##' @return a data.frame or a character if the loading failed
##' @export
##' @importFrom data.table fread
##' @importFrom dplyr select
##' @importFrom cli cli_alert_warning

load_gbif_data <- function(species.name, folder.gbif, filter.atlas, all.columns = FALSE){
  .load_gbif_data.check.args(species.name = species.name,
                             folder.gbif = folder.gbif,
                             filter.atlas = filter.atlas, 
                             all.columns = all.columns)
  all.files <- list.files(path = folder.gbif, include.dirs = TRUE, recursive = TRUE)
  selected.files <- all.files[grepl(x = all.files, pattern = ".csv", fixed = TRUE)]
  selected.files <- selected.files[grepl(x = selected.files, pattern = species.name, fixed = TRUE)]
  if (length(selected.files) == 0) {
    return("No file Found")
  } else if (length(selected.files) > 1) {
    return("Multiple files found")
  } else {
    test.read <- try({
      output <- fread(file = paste0(folder.gbif, "/", selected.files), 
                      showProgress = FALSE)
    })
    if (inherits(test.read, "try-error")) {
      return("File could not be read")
    } 
    if (nrow(output) == 0) {
      return("File have no occurrences")
    }
    if (any(grepl(pattern = "geometry", x = colnames(output)))) {
      cli_alert_warning("Reprojecting species {species.name}")
      this.geometry <- output$geometry
      this.X <- sapply(strsplit(this.geometry, split = "|", fixed = TRUE), 
                       function(x){ as.numeric(x[1]) })
      this.Y <- sapply(strsplit(this.geometry, split = "|", fixed = TRUE), 
                       function(x){ as.numeric(x[2]) })
      mat.proj <- 
        project(cbind(this.X,this.Y), from = crs("EPSG:3035"), to = crs("EPSG:4326"))
      output$geometry <- NULL
      output$X <- mat.proj[,1]
      output$Y <- mat.proj[,2]
      # fwrite(x = output, file = paste0(folder.gbif, selected.files),
      #        showProgress = FALSE, sep = ";")
    }
    
    test.colnames <- try({
      .fun_testdfcolnames(output,
                          thiscolnames = c("species",
                                           "coordinateUncertaintyInMeters",
                                           "coordinatePrecision", 
                                           "distance_to_iucn",
                                           "X",	"Y"))
    })
    if (inherits(test.colnames, "try-error")) {
      return("Column names not found")
    } 
    
    if (any(output$species != species.name)) {
      # return("Error in species name")
      # in case harmonization is required in the future
      output <- mutate(output, species = species.name)
    }
    
    output <- 
      output %>% 
      filter(X >= -180, 
             X <= 180,
             Y >= -90,
             Y <= 90)
    if (nrow(output) == 0) {
      return("Error with Latitude/Longitude")
    } 
    
    if (filter.atlas) {
      output <- filter(output,
                       is.na(coordinateUncertaintyInMeters) |
                         coordinateUncertaintyInMeters <= 5000)
    }
    
    if (all.columns) return(output)
    
    select(output, species, coordinateUncertaintyInMeters, coordinatePrecision, distance_to_iucn, X, Y)
  }
}


.load_gbif_data.check.args <- function(species.name, folder.gbif,
                                       filter.atlas, all.columns){
  .fun_testIfInherits(species.name, "character")
  .fun_testIfInherits(folder.gbif, "character")
  .fun_testIfDirExists(folder.gbif)
  stopifnot(is.logical(filter.atlas))
  stopifnot(is.logical(all.columns))
  TRUE
}


## Locate IUCN distribution ----------------------------
##' @name locate_iucn_distribution
##' 
##' @title Locate IUCN distribution
##' 
##' @description \code{locate_iucn_distribution} find distribution for a given
##'  species within \code{folder.iucn}. Path to file is then returned
##' 
##' @inheritParams gbif_outsider
##' @param species.code a \code{character} corresponding to the species code
##' @param filetype a \code{character} (\emph{default} \code{'.tif'}) determining
##' the file extension to be matched
##' @return a data.frame or a character if the loading failed
##' @export

locate_iucn_distribution <- function(species.code, folder.iucn, filetype = ".tif"){
  .locate_iucn_distribution.check.args(species.code = species.code,
                                       folder.iucn = folder.iucn)
  all.files <- list.files(path = folder.iucn, include.dirs = TRUE, recursive = TRUE)
  selected.files <- all.files[grepl(x = all.files, pattern = filetype, fixed = TRUE)]
  selected.files <- selected.files[grepl(x = selected.files, pattern = species.code, fixed = TRUE)]
  if (length(selected.files) == 0) {
    return(NULL)
  } 
  paste0(folder.iucn, "/", selected.files)
}


.locate_iucn_distribution.check.args <- function(species.code, folder.iucn){
  .fun_testIfInherits(species.code, "character")
  .fun_testIfInherits(folder.iucn, "character")
  .fun_testIfDirExists(folder.iucn)
  TRUE
}



### load_data.trophic_dataset    --------------------------------------------------
##'
##' @rdname trophic_dataset
##' @param x an object of class \code{trophic_dataset}
##' @export
##'


setGeneric("load_data", def = function(x, ...) {
  standardGeneric("load_data") 
})

##' @rdname trophic_dataset
##' @export
##' @importFrom cli cli_alert_success
##' @param SpeciesName a \code{character}, a species name to retrieve data or 
##' information
##' @param Code a \code{character}, a species code to retrieve data or information
##' @param type a \code{character}, the type of data to retrieve: trophic, trophic.raw
##' occurrence

setMethod('load_data', signature(x = 'trophic_dataset'),
          function(x,
                   SpeciesName,
                   Code, 
                   type = "trophic") {
            SpeciesName <- .load_data.check.args(x = x,
                                                 SpeciesName = SpeciesName,
                                                 Code = Code,
                                                 type = type)
            if (type == "trophic") {
              df <- fread(x@files@trophic[SpeciesName], showProgress = FALSE)
            } else if (type == "trophic.raw") {
              df <- fread(x@files@trophic.raw[SpeciesName], showProgress = FALSE)
            } else if (type == "occurrence") {
              df <- fread(x@files@occurrence[SpeciesName], showProgress = FALSE)
            }
            df
          })



### load data argument check --------------------------------------------------
.load_data.check.args <- function(x, SpeciesName, Code, type){
  
  if (missing(type)) {
    type <- "trophic"
    cli_alert_info('Retrieving trophic dataset')
  }
  
  .fun_testIfIn(type, c("trophic","occurrence","trophic.raw"))
  if (type == "trophic") {
    summary.occ <- x@summary@trophic
  } else if (type == "trophic.raw") {
    summary.occ <- x@summary.raw@trophic
  } else if (type == "occurrence") {
    summary.occ <- x@summary@occurrence
  }
  
  if (!xor(missing(SpeciesName),missing(Code))) {
    stop("Exactly one of SpeciesName or Code must be given as argument")
  }
  
  
  
  if (!missing(Code)) {
    listname <- summary.occ$SpeciesName
    names(listname) <- summary.occ$Code
    if (!Code %in% summary.occ$Code) {
      stop(paste0("dataset for ", Code, " not found"))
    }
    SpeciesName <- listname[Code]
  } 
  
  if (!missing(SpeciesName)) {
    if (!SpeciesName %in% summary.occ$SpeciesName) {
      stop(paste0("dataset for ", SpeciesName, " not found"))
    }
  }
  return(SpeciesName)
}



## Load dataset ----------------------------
##' @name load_dataset
##' 
##' @title Load trophic dataset
##' 
##' @description \code{load_dataset} loads a trophic dataset
##' 
##' @inheritParams gbif_outsider
##' @return a \code{\link{trophic_dataset}} 
##' @export

load_dataset <- function(project.name){
  .fun_testIfInherits(project.name, 'character')
  .fun_testIfDirExists(project.name)
  this.file <- paste0(project.name,"/",project.name,".trophic_dataset.rds")
  if (file.exists(this.file)) {
    return(readRDS(this.file))
  } else {
    cli_alert_danger("Dataset not found")
    return(invisible(NULL))
  }
}
