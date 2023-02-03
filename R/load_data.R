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

load_gbif_data <- function(species.name, folder.gbif){
  .load_gbif_data.check.args(species.name = species.name,
                             folder.gbif = folder.gbif)
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
    
    test.colnames <- try({
      .fun_testdfcolnames(output,
                          thiscolnames = c("species",
                                           "coordinatePrecision", 
                                           "distance_to_iucn",
                                           "X",	"Y"))
    })
    if (inherits(test.colnames, "try-error")) {
      return("Column names not found")
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
    select(output, species,coordinatePrecision,distance_to_iucn,X,Y)
  }
}


.load_gbif_data.check.args <- function(species.name, folder.gbif){
  .fun_testIfInherits(species.name, "character")
  .fun_testIfInherits(folder.gbif, "character")
  .fun_testIfDirExists(folder.gbif)
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
