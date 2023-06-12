## --------------------------------------------------------------------------- #
# 1. map_effort         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name map_effort
##' @author Remi Patin
##'
##' @title Plot sampling effort
##'
##' @description \code{plot_effort} represent the sampling effort based on 
##' administrative boundaries (country or regions)
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @param vector_data a \code{SpatVector} with administrative boundary
##' @param suffix a \code{character} with the name of the administrative boundary
##' @importFrom terra vect crop rast writeVector
##' @importFrom tidyterra geom_spatvector 
##' @importFrom ggplot2 ggplot scale_fill_brewer ggtitle
##' @importFrom cowplot save_plot
##' @importFrom cli cli_progress_step cli_progress_done

map_effort <- function(project.name, vector_data, data.mask, suffix){
  
  
  myBreaks_tot <- c(0, 1e-3, 1e-2, 0.1, 0.5, 1, 5, 10, 100, Inf)
  myBreaks_end <- myBreaks_tot[-1]
  myBreaks_begin <- myBreaks_tot[-length(myBreaks_tot)]
  myBreaks_label <- paste0("[", myBreaks_begin, "-", myBreaks_end,"]")

  
  
  all.files <- list.files(project.name, pattern = ".tif")
  if (length(all.files) > 0) {
    
    store.dir <- paste0(project.name,"/plot/", suffix,"/")
    if (!dir.exists(store.dir)) dir.create(store.dir, recursive = TRUE)
    shp.dir <- paste0(project.name,"/plot/shapefiles/")
    if (!dir.exists(shp.dir)) dir.create(shp.dir)
    
    layer.names <-
      all.files %>%
      gsub(pattern = "sampling.effort.",
           replacement = "",
           x = .) %>% 
      gsub(pattern = ".tif",
           replacement = "",
           x = .)
    # this.layer <- "Amphibia"
    for (this.layer in layer.names) {
      cli_progress_step(this.layer)
      this.rast <- rast(paste0(project.name, "/sampling.effort.", this.layer, ".tif"))  
      this.mean <- extract(this.rast, vector_data, fun = "mean", na.rm = TRUE)
      this.vector <- vector_data
      # this.vector$effort <- this.mean[, gsub(pattern = " ",
      #                                        replacement = ".",
      #                                        x = this.layer)]
      this.vector$effort <- this.mean[, this.layer]
      this.vector <- crop(this.vector, data.mask)
      
      this.vector$effort_char <- factor(myBreaks_label[findInterval(this.vector$effort,
                                                                    vec = myBreaks_tot)],
                                        levels = myBreaks_label)
      
      # export shape
      this.file <- paste0(shp.dir,"/",this.layer,".",suffix,".shp")
      if (file.exists(this.file)) file.remove(this.file)
      writeVector(this.vector, filename = this.file)
      
      g <- ggplot() +
        geom_spatvector(data = this.vector, aes(fill = effort_char)) + 
        scale_fill_brewer(
          "Mean occurrence per cell",
          palette = "RdYlBu",
          breaks = c(rev(myBreaks_label),NA),
          drop = FALSE
        ) +
        ggtitle(paste0("Sampling effort for ", this.layer))
      
      save_plot(filename = paste0(store.dir,"/",this.layer,".",suffix,".png"),
                plot = g, base_width = 30/cm(1), base_height = 20/cm(1))
    }
    cli_progress_done()
  }
  invisible(NULL)
}

## --------------------------------------------------------------------------- #
# 2. map_uncertain         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name map_uncertain
##' @author Remi Patin
##'
##' @title Plot absence uncertainty
##'
##' @description \code{map_uncertain} represent the mean probability for an absence
##' to be classified as uncertain
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @inheritParams map_effort
##' @param vector_data a \code{SpatVector} with administrative boundary
##' @param suffix a \code{character} with the name of the administrative boundary
##' @importFrom terra vect crop rast writeVector
##' @importFrom tidyterra geom_spatvector 
##' @importFrom ggplot2 ggplot scale_fill_brewer ggtitle
##' @importFrom cowplot save_plot
##' @importFrom cli cli_progress_step cli_progress_done

map_uncertain <- function(project.name, vector_data, data.mask, suffix){
  
  
  myBreaks_tot <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1)
  myBreaks_end <- myBreaks_tot[-1]
  myBreaks_begin <- myBreaks_tot[-length(myBreaks_tot)]
  myBreaks_label <- paste0("[", myBreaks_begin, "-", myBreaks_end,"]")
  
  
  
  all.files <- list.files(project.name, pattern = "summary.uncertain")
  if (length(all.files) > 0) {
    
    store.dir <- paste0(project.name,"/plot/", suffix,"/")
    if (!dir.exists(store.dir)) dir.create(store.dir, recursive = TRUE)
    shp.dir <- paste0(project.name,"/plot/shapefiles/")
    if (!dir.exists(shp.dir)) dir.create(shp.dir)
    
    layer.names <-
      all.files %>%
      gsub(pattern = "summary.uncertain_",
           replacement = "",
           x = .) %>% 
      gsub(pattern = ".tif",
           replacement = "",
           x = .)
    # this.layer <- "Amphibia"
    for (this.layer in layer.names) {
      cli_progress_step(this.layer)
      this.rast <- rast(paste0(project.name, "/summary.uncertain_", this.layer, ".tif"))  
      this.mean <- extract(this.rast, vector_data, fun = "mean", na.rm = TRUE)
      this.vector <- vector_data
      this.vector$prop <- this.mean[, "layer"]
      this.vector <- crop(this.vector, data.mask)
      
      this.vector$effort_char <- factor(myBreaks_label[findInterval(this.vector$prop, vec = myBreaks_tot)],
                                        levels = rev(myBreaks_label))
      
      # export shape
      this.file <- paste0(shp.dir,"/uncertain.",this.layer,".",suffix,".shp")
      if (file.exists(this.file)) file.remove(this.file)
      writeVector(this.vector, filename = this.file)
      
      g <- ggplot() +
        geom_spatvector(data = this.vector, aes(fill = effort_char)) + 
        scale_fill_brewer(
          "Probability",
          # direction = -1,
          palette = "RdYlBu",
          breaks = c(myBreaks_label, NA),
          drop = FALSE
        ) +
        ggtitle(paste0("Prob. of absence uncertainty - ", this.layer))
      
      save_plot(filename = paste0(store.dir,"/uncertain.",this.layer,".",suffix,".png"),
                plot = g, base_width = 30/cm(1), base_height = 20/cm(1))
    }
    cli_progress_done()
  }
  invisible(NULL)
}



## --------------------------------------------------------------------------- #
# 2. map_prey_filtering         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name map_prey_filtering
##' @author Remi Patin
##'
##' @title Information on cells filtered due to prey
##'
##' @description \code{map_prey_filtering} represent the mean probability of
##' filtering a cell due to uncertain prey with different aggregation levels
##'
##' @inheritParams gbif_outsider
##' @inheritParams taxonomic_conflict
##' @inheritParams sampling_effort
##' @inheritParams map_effort
##' @param vector_data a \code{SpatVector} with administrative boundary
##' @param suffix a \code{character} with the name of the administrative boundary
##' @importFrom terra vect crop rast writeVector
##' @importFrom tidyterra geom_spatvector 
##' @importFrom ggplot2 ggplot scale_fill_brewer ggtitle
##' @importFrom cowplot save_plot
##' @importFrom cli cli_progress_step cli_progress_done

map_prey_filtering <- function(project.name, vector_data, data.mask, suffix){
  
  
  myBreaks_tot <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1)
  myBreaks_end <- myBreaks_tot[-1]
  myBreaks_begin <- myBreaks_tot[-length(myBreaks_tot)]
  myBreaks_label <- paste0("[", myBreaks_begin, "-", myBreaks_end,"]")
  
  
  
  all.files <- list.files(project.name, pattern = "summary.filtered")
  if (length(all.files) > 0) {
    
    store.dir <- paste0(project.name,"/plot/", suffix,"/")
    if (!dir.exists(store.dir)) dir.create(store.dir, recursive = TRUE)
    shp.dir <- paste0(project.name,"/plot/shapefiles/")
    if (!dir.exists(shp.dir)) dir.create(shp.dir)
    
    layer.names <-
      all.files %>%
      gsub(pattern = "summary.filtered_",
           replacement = "",
           x = .) %>% 
      gsub(pattern = ".tif",
           replacement = "",
           x = .)
    # this.layer <- "Amphibia"
    for (this.layer in layer.names) {
      cli_progress_step(this.layer)
      this.rast <- rast(paste0(project.name, "/summary.filtered_", this.layer, ".tif"))  
      this.mean <- extract(this.rast, vector_data, fun = "mean", na.rm = TRUE)
      this.vector <- vector_data
      this.vector$prop <- this.mean[, "layer"]
      this.vector <- crop(this.vector, data.mask)
      
      this.vector$effort_char <- factor(myBreaks_label[findInterval(this.vector$prop, vec = myBreaks_tot)],
                                        levels = rev(myBreaks_label))
      
      # export shape
      this.file <- paste0(shp.dir,"/filtered.",this.layer,".",suffix,".shp")
      if (file.exists(this.file)) file.remove(this.file)
      writeVector(this.vector, filename = this.file)
      
      g <- ggplot() +
        geom_spatvector(data = this.vector, aes(fill = effort_char)) + 
        scale_fill_brewer(
          "Probability",
          # direction = -1,
          palette = "RdYlBu",
          breaks = c(myBreaks_label, NA),
          drop = FALSE
        ) +
        ggtitle(paste0("Prob. of filtering cell due to prey uncertainty  - ", this.layer))
      
      save_plot(filename = paste0(store.dir,"/prey.filtering.",this.layer,"_",suffix,".png"),
                plot = g, base_width = 30/cm(1), base_height = 20/cm(1))
    }
    cli_progress_done()
  }
  invisible(NULL)
}