## --------------------------------------------------------------------------- #
# 1. check_gbif_duplicata         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name check_gbif_duplicata
##' @author Remi Patin
##'
##' @title Check gbif duplicated data
##'
##' @description \code{check_gbif_duplicata} retrieve the number of data in each
##' gbif file and return the file with the exact same number of data to 
##' identify gbif data that may be duplicated between species considered in gbif
##' as subspecies.
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @inheritParams .register_cluster
##' @importFrom cli cli_progress_step cli_progress_done
##' @importFrom foreach foreach `%dopar%`
##' @importFrom data.table fread
##' @importFrom dplyr select mutate full_join group_by

check_gbif_duplicata <- function(checklist, 
                                 folder.gbif,
                                 nb.cpu = 1){
  .check_checklist(checklist)
  .fun_testIfInherits(folder.gbif, "character")
  .fun_testIfDirExists(folder.gbif)
  .fun_testIfPosInt(nb.cpu)
  has.cluster <- .register_cluster(nb.cpu)
  df.nrow <- foreach(this.species = checklist$SpeciesName, .combine = 'rbind') %dopar% {
    cli_progress_step(this.species)
    all.files <- list.files(path = folder.gbif, include.dirs = TRUE, recursive = TRUE)
    selected.files <- all.files[grepl(x = all.files, pattern = ".csv", fixed = TRUE)]
    selected.files <- selected.files[grepl(x = selected.files, pattern = this.species, fixed = TRUE)]
    
    if (length(selected.files) == 0) {
      this.nrow <- 0
    } else if (length(selected.files) > 1) {
      this.nrow <- NA
    } else {
      test.read <- try({
        output <- fread(file = paste0(folder.gbif, "/", selected.files), 
                        showProgress = FALSE)
      })
      if (inherits(test.read, "try-error")) {
        this.nrow <- NA
      } else {
        this.nrow <- nrow(output)
        rm(output); gc()
      }
    }
    cli_progress_done()
    return(data.frame(
      SpeciesName = this.species,
      nrow = this.nrow
    ))
  }
  df.res <- checklist %>% 
  select(SpeciesName, Class, URI_GBIF) %>% 
    full_join(df.nrow, by = "SpeciesName") %>% 
    select(Class, SpeciesName, nrow, URI_GBIF)
  df.res %>% 
    filter(nrow > 0) %>% 
    group_by(Class, nrow) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(n > 1)
}