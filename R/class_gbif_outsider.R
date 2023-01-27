## --------------------------------------------------------------------------- #
# 1. gbif_outsider         ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name gbif_outsider
##' @aliases gbif_outsider-class
##' @author Remi Patin
##' 
##' @title Object class containing gbif observation outside of IUCN range
##' 
##' @description Class returned by \code{\link{detect_gbif_outsider}}, 
##' that list gbif observations outside of IUCN range. Also provides tools to 
##' explore the distribution of outsiders.
##' 
##' 
##' @inheritParams taxonomic_conflict
##' @param folder.gbif a \code{character}: folders in which gbif data are stored
##' @param folder.iucn a \code{character}: folders in which IUCN distributions
##'  are stored
##' @slot outsider a \code{data.frame} with all gbif outsider
##' @slot data.summary a \code{data.frame} summarising the total number of gbif 
##' data for each species
##' @slot iucn.distrib a \code{list} with path for iucn distribution for each species
##' @slot checklist a \code{data.frame} with information on all species of interest
##' @slot project.name a \code{character} indicating the folder in which logfiles 
##' and data may be written
##' @slot species.failed a \code{character} listing all species for which no 
##' data were found
##' 
##' @examples
##' 
##' showClass("gbif_outsider")
NULL

##' @name gbif_outsider-class
##' @rdname gbif_outsider
##' @export
##' @importFrom terra rast

## 1.1 Class Definition ----------------------------------------------------------------------------

setClass("gbif_outsider",
         representation(outsider = 'data.frame',
                        data.summary = 'data.frame',
                        iucn.distrib = 'list',
                        checklist = 'data.frame',
                        project.name = 'character',
                        species.failed = 'data.frame'),
         validity = function(object){ 
           .check_checklist(object@checklist)
           .fun_testIfInherits(project.name, "character")
           .fun_testIfInherits(object@outsider, "data.frame")
           .fun_testIfInherits(object@data.summary, "data.frame")
           TRUE
         })

## 1.2 Constructors -------------------------------------------------------------

### detect_gbif_outsider -----------------------------------
##' 
##' @rdname gbif_outsider
##' @export
##' @importFrom foreach foreach %dopar% 
##' @importFrom cli cli_progress_step cli_progress_done
##' @importFrom data.table rbindlist first
##' @importFrom dplyr mutate



detect_gbif_outsider <- function(checklist, folder.gbif, folder.iucn, 
                                 project.name, nb.cpu = 1){
  
  .detect_gbif_outsider.check.args(checklist = checklist,
                                   folder.gbif = folder.gbif,
                                   folder.iucn = folder.iucn,
                                   project.name = project.name,
                                   nb.cpu = nb.cpu)
  
  .register_cluster(nb.cpu = nb.cpu)
  
  listcode <- checklist$Code
  names(listcode) <- checklist$SpeciesName
  
  output.outsider <- foreach(this.species = names(listcode)) %dopar% {
    cli_progress_step(this.species)
    this.output <- load_gbif_data(species.name = this.species,
                                  folder.gbif = folder.gbif)
    if (!inherits(this.output,"data.frame")) {
      cli_progress_done()
      return(list("status" = "failed",
                  "species" = this.species,
                  "species.iucn" = NULL,
                  "results" = this.output))
    } else {
      total.gbif <- nrow(this.output)
      if (first(this.output$distance_to_iucn) == "No distribution found") {
        this.output <- 
          mutate(this.output, distance_to_iucn = 0)
        species.iucn <- NULL
      } else {
        this.output <- 
          mutate(this.output, distance_to_iucn = as.numeric(distance_to_iucn))
        species.iucn <-
          locate_iucn_distribution(species.code = listcode[this.species],
                                   folder.iucn = folder.iucn)
      }
      this.output <- 
        filter(this.output, distance_to_iucn > 0)
      cli_progress_done()
      return(list("status" = "success",
                  "species" = this.species,
                  "species.iucn" = species.iucn,
                  "results" = this.output, 
                  "total.gbif" = total.gbif))
    }
  }
  failed.df <- lapply(output.outsider, function(x){
    if (x$status == "failed"){
      return(data.frame(species = x$species, results = x$results))
    }
    NULL
  }) %>% rbindlist()
  
  summary.df <- lapply(output.outsider, function(x){
    if (x$status == "success"){
      return(data.frame("species" = x$species,
                        "outside.iucn" = nrow(x$results),
                        "total.gbif" = x$total.gbif))
    }
    NULL
  }) %>% rbindlist()
  
  
  success.df <- lapply(output.outsider, function(x){
    if (x$status == "success"){
      return(x$results)
    }
    NULL
  }) %>% rbindlist()
  
  iucn.list <- lapply(output.outsider, function(x){
    if (x$status == "success"){
      if(length(x$species.iucn) > 1){
        x$species.iucn <- "multiple files"
      } 
      return(x$species.iucn)
    }
    NULL
  })
  names(iucn.list) <- listcode
  
  summary2.df <- success.df %>%
    group_by(species) %>%
    summarise(mean_distance_to_iucn = mean(distance_to_iucn),
              median_distance_to_iucn = mean(distance_to_iucn),
              q5_distance_to_iucn = quantile(distance_to_iucn, probs = 0.05),
              q25_distance_to_iucn = quantile(distance_to_iucn, probs = 0.25),
              q75_distance_to_iucn = quantile(distance_to_iucn, probs = 0.75),
              q95_distance_to_iucn = quantile(distance_to_iucn, probs = 0.95))
  
  output.summary <- 
    left_join(summary.df, summary2.df, by = "species") %>% 
    left_join(checklist, by = c("species" = "SpeciesName")) %>% 
    mutate(prop.outside = round(outside.iucn/total.gbif*100, digits = 2))

  output.success <- 
    left_join(success.df, checklist, by = c("species" = "SpeciesName"))
  output.failed <- 
    left_join(failed.df, checklist, by = c("species" = "SpeciesName"))
  
  output <- new("gbif_outsider")
  output@outsider <- output.success
  output@data.summary <- output.summary
  output@iucn.distrib <- iucn.list
  output@checklist <- checklist
  output@project.name <- project.name
  output@species.failed <- output.failed
  output
}

### Argument Check -----------------------------------

.detect_gbif_outsider.check.args <- function(checklist, folder.gbif, folder.iucn,
                                             project.name, nb.cpu = nb.cpu){
  .check_checklist(checklist)
  .fun_testIfInherits(folder.gbif, "character")
  .fun_testIfDirExists(folder.gbif)
  .fun_testIfInherits(folder.iucn, "character")
  .fun_testIfDirExists(folder.iucn)
  .fun_testIfPosInt(nb.cpu)
  TRUE
}

## 1.3 Methods -------------------------------------------------------------
### show.gbif_outsider    --------------------------------------------------
##' 
##' @rdname gbif_outsider
##' @importMethodsFrom methods show
##' @param object an object of class \code{gbif_outsider}
##' @export
##' 

setMethod('show', signature('gbif_outsider'),
          function(object)
          {
            goodspecies <- 
              nrow(object@checklist) - 
              nrow(object@data.summary) - 
              nrow(object@species.failed)
            median.dist <- median(object@outsider$distance_to_iucn)
            median.prop <- median(object@data.summary$outside.iucn /
                                    object@data.summary$total.gbif)
            cat("\n Project: ",object@project.name)
            cat("\n",  nrow(object@data.summary), 
                " species with gbif data outside IUCN ranges")
            cat("\n", goodspecies, 
                " species for which all gbif data were inside IUCN ranges (or without IUCN ranges")
            cat("\n", nrow(object@species.failed), 
                " species for which gbif data could not be retrieved")
            cat("\n", "Median distance to IUCN ranges across all data:",
                round(median.dist),"m")
            cat("\n", "Median proportion of points outside IUCN ranges across species:",
                round(median.prop*100, digits = 2),"%")
            invisible(NULL)
          })

### summary.gbif_outsider    --------------------------------------------------
##' 
##' @rdname gbif_outsider
##' @inheritParams taxonomic_conflict
##' @export
##' @importFrom dplyr group_by summarise arrange across right_join
##' @importFrom magrittr "%>%"
##' 


summary_outsider <- function(object, type = "Class") {
  .fun_testIfIn(type, c("Class","Order","Family","Species"))
  .fun_testIfInherits(object, "gbif_outsider")
  
  if(type == "Species"){
    return(object@data.summary)
  }
  
  object.summary <- object@outsider  %>% 
    group_by(Class, Order, Family, Code) %>% 
    summarise(median = median(distance_to_iucn),
              .groups = "drop") %>% 
    group_by(across(type)) %>% 
    summarise(
      q5 = quantile(median, probs = 0.05),
      q25 = quantile(median, probs = 0.25),
      q75 = quantile(median, probs = 0.75),
      q95 = quantile(median, probs = 0.95),
      median = median(median),
      .groups = "drop")
  if(type == "Order"){
    object.summary <- object@outsider %>% 
      group_by(Class, Order) %>% 
      summarise(Code = first(Code),
                .groups = "drop") %>%
      select(-Code) %>% 
      right_join(., object.summary, by = "Order") %>% 
      arrange(Class, Order)
  }
  if(type == "Family"){
    object.summary <- object@outsider %>% 
      group_by(Class, Order, Family) %>% 
      summarise(Code = first(Code),
                .groups = "drop") %>%
      select(-Code) %>%
      right_join(., object.summary, by = "Family") %>% 
      arrange(Class, Order, Family)
  }
  object.summary
}

### plot.gbif_outsider  --------------------------------------------------
##' 
##' @rdname gbif_outsider
##' @export
##' @param plot.folder a path to store plot results when \code{type = "Species"}
##' @importFrom ggplot2 ggplot geom_boxplot aes ggtitle scale_y_log10
##'  scale_fill_discrete scale_x_discrete theme geom_point scale_fill_manual
##' scale_color_continuous ggtitle
##' @importFrom tidyterra geom_spatraster mutate  
##' @importFrom scales cut_short_scale label_number
##' @importFrom dplyr mutate filter
##' @importFrom magrittr "%>%"
##' @importFrom foreach foreach "%dopar%"
##' @importFrom cowplot save_plot

setMethod('plot', signature(x = 'gbif_outsider', y = 'missing'),
          function(x, type = "Class", nb.cpu = 1, plot.folder = "gbif_outsider")
          {
            type.range <- c("Class","Order","Family", "Species")
            .fun_testIfIn(type, type.range)
            .fun_testIfPosInt(nb.cpu)
            .register_cluster(nb.cpu = nb.cpu)
            
            x.summary <- summary_outsider(x, type = type)
            if (type != "Species") {
              type.fill <- type.range[max(1, which(type.range == type)-1)]
              g <- ggplot(x.summary)+
                geom_boxplot(aes(x = get(type),
                                 fill = get(type.fill),
                                 ymin = q5, ymax = q95,
                                 lower = q25, upper = q75,
                                 middle = median), stat = "identity") +
                scale_y_log10(
                  "Distance to IUCN distribution",
                  label = label_number(suffix = "m", 
                                       scale_cut = cut_short_scale()[1:2]))+
                scale_fill_discrete(type.fill)+
                theme(axis.text.x = element_text(angle = 90,
                                                 hjust = 1,
                                                 vjust = 0.5))
              
              if(type == "Class"){
                g <- g +
                  scale_x_discrete(type, limits = unlist(x.summary[,type]))
              } else {
                g <- g + scale_x_discrete(type)
              }
              
              if (type == "Family") {
                g <- g + 
                  facet_grid(~Class, scales = "free", space = "free")
              }
              return(g)
            } 
            # Species specifc maps plot
            dir.create(plot.folder, recursive = TRUE, showWarnings = FALSE)
            x.summary %>% 
              filter(outside.iucn > 0)
            listname <- x.summary$species
            names(listname) <- x.summary$Code
            out.list <- foreach( thiscode = names(listname) ) %dopar% {
              cli::cli_progress_step(listname[thiscode])
              path.file <- paste0(plot.folder, "/", thiscode,".png")
              df <- x@outsider %>% 
                filter(Code == thiscode)
              this.summary <- filter(x.summary, Code == thiscode)
              this.iucn <- x@iucn.distrib[[thiscode]]
              g <- ggplot()
              if (!is.null(this.iucn) && 
                  grepl(x = this.iucn, pattern = ".tif")) {
                this.distrib <- rast(this.iucn)
                capture.output({
                  g <- ggplot(df)+
                    geom_spatraster(
                      data = mutate(this.distrib, 
                                    presence = factor(presence))
                    ) +
                    scale_fill_manual(
                      "IUCN distribution",
                      values = c("#d95f02",
                                 "#7570b3",
                                 "#1b9e77",
                                 "#e7298a",
                                 "#66a61e"),
                      na.value = "grey")
                }, type = "message")
              }
              if (nrow(df) > 0) {
                g <- g +
                  geom_point(data = df,
                             aes(x = X, y = Y, color = distance_to_iucn), 
                             shape = 20)+
                  scale_color_continuous("Distance to IUCN distribution")
              }
              g <- g + ggtitle(
                paste0(
                  listname[thiscode]," (",
                  this.summary$Order, ", ",
                  this.summary$Family, ") - ", 
                  this.summary$outside.iucn, "/",
                  this.summary$total.gbif, " (",
                  this.summary$prop.outside,"%)")
              )
              save_plot(filename = path.file,
                        g, base_height = 20/cm(1), base_width = 30/cm(1))
              return(NULL)
            }
            invisible(NULL)
          })


