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
##' @slot gbif.failed a \code{data.frame} listing all species for which no 
##' gbif data were found
##' @slot iucn.failed a \code{data.frame} listing all species for which no 
##' iucn distribution were found
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
                        gbif.failed = 'data.frame',
                        iucn.failed = 'data.frame'),
         validity = function(object){ 
           .check_checklist(object@checklist)
           .fun_testIfInherits(object@project.name, "character")
           .fun_testIfInherits(object@outsider, "data.frame")
           .fun_testIfInherits(object@data.summary, "data.frame")
           TRUE
         })

## 1.2 Constructors -------------------------------------------------------------

### detect_gbif_outsider -----------------------------------
##' 
##' @rdname gbif_outsider
##' @export
##' @inheritParams .register_cluster
##' @importFrom foreach foreach %dopar% 
##' @importFrom cli cli_progress_step cli_progress_done cli_h1 cli_h2
##' @importFrom data.table rbindlist 
##' @importFrom dplyr mutate first
##' @importFrom stats quantile median
##' @importFrom utils capture.output



detect_gbif_outsider <- function(checklist, folder.gbif, folder.iucn, 
                                 filter.atlas = FALSE,
                                 project.name, nb.cpu = 1){
  
  cli_h1("Detect gbif outsider")
  cli_progress_step("Argument check")
  .detect_gbif_outsider.check.args(checklist = checklist,
                                   folder.gbif = folder.gbif,
                                   folder.iucn = folder.iucn,
                                   filter.atlas = filter.atlas,
                                   project.name = project.name,
                                   nb.cpu = nb.cpu)
  
  has.cluster <- .register_cluster(nb.cpu = nb.cpu)
  
  listcode <- checklist$Code
  names(listcode) <- checklist$SpeciesName
  
  cli_progress_done()
  cli_h2("Loop on each species")
  ## Species Loop ------------------------------------------------------------
  if(has.cluster) cli_progress_step("Loop on species with a {nb.cpu}-core cluster")
  output.outsider <- foreach(this.species = names(listcode)) %dopar% {
    cli_progress_step(this.species)
    this.output <- load_gbif_data(species.name = this.species,
                                  folder.gbif = folder.gbif, 
                                  filter.atlas = filter.atlas)
    if (!inherits(this.output,"data.frame")) {
      cli_progress_done()
      return(list("status" = "failed",
                  "species" = this.species,
                  "species.iucn" = NULL,
                  "results" = this.output))
    } else {
      total.gbif <- nrow(this.output)
      species.iucn <-
        locate_iucn_distribution(species.code = listcode[this.species],
                                 folder.iucn = folder.iucn)
      if (first(this.output$distance_to_iucn) == "No distribution found" ||
          is.null(species.iucn)) {
        this.output <- 
          mutate(this.output, distance_to_iucn = 0)
      } else {
        this.output <- 
          mutate(this.output, distance_to_iucn = as.numeric(distance_to_iucn))
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
  cli_progress_done()
  
  
  ## Summary ------------------------------------------------------------
  
  cli_h2("Extract summary")
  cli_progress_step("Failed Species")
  failed.df <- lapply(output.outsider, function(x){
    if (x$status == "failed") {
      return(data.frame(species = x$species, results = x$results))
    }
    NULL
  }) %>% rbindlist()
  
  
  
  
  cli_progress_step("Successfull species")
  success.df <- lapply(output.outsider, function(x){
    if (x$status == "success") {
      return(x$results)
    }
    NULL
  }) %>% rbindlist()
  
  
  cli_progress_step("Link to IUCN files")
  iucn.list <- lapply(output.outsider, function(x){
    if (x$status == "success" ) {
      if (length(x$species.iucn) > 1) {
        x$species.iucn <- "multiple files"
      } 
      return(x$species.iucn)
    }
    NULL
  })
  names(iucn.list) <- listcode
  
  cli_progress_step("Summary for successfull species")
  summary.df <- lapply(output.outsider, function(x){
    if (x$status == "success") {
      return(data.frame("species" = x$species,
                        "outside.iucn" = nrow(x$results),
                        "total.gbif" = x$total.gbif))
    }
    NULL
  }) %>% rbindlist()
  
  summary2.df <- success.df %>%
    group_by(species) %>%
    summarise(mean = round(mean(distance_to_iucn)),
              median = round(median(distance_to_iucn)),
              q5 = round(quantile(distance_to_iucn, probs = 0.05)),
              q25 = round(quantile(distance_to_iucn, probs = 0.25)),
              q75 = round(quantile(distance_to_iucn, probs = 0.75)),
              q80 = round(quantile(distance_to_iucn, probs = 0.80)),
              q85 = round(quantile(distance_to_iucn, probs = 0.85)),
              q90 = round(quantile(distance_to_iucn, probs = 0.90)),
              q95 = round(quantile(distance_to_iucn, probs = 0.95)))
  
  output.summary <- 
    left_join(summary.df, summary2.df, by = "species") %>% 
    mutate(prop.outside = round(outside.iucn/total.gbif*100, digits = 2)) %>% 
    left_join(checklist, by = c("species" = "SpeciesName")) 
  
  output.success <- 
    left_join(success.df, checklist, by = c("species" = "SpeciesName"))
  output.failed <- 
    left_join(failed.df, checklist, by = c("species" = "SpeciesName"))
  
  ## Logs ------------------------------------------------------------
  cli_progress_step("Write logs")
  iucn.failed <- 
    do.call('c',
            sapply(iucn.list, function(x){
              if(is.null(x)){ 
                return("No distribution found")
              }
              if(length(x) > 1){
                return("Multiple distribution found")
              }
              NULL
            }))
  iucn.failed.df <- 
    data.frame(
      "Code" = names(iucn.failed),
      "SpeciesName" =  
        sapply(names(iucn.failed), 
               function(x) checklist$SpeciesName[which(checklist$Code == x)])
      , 
      "failed.iucn"= iucn.failed) %>% 
    left_join( checklist, by = c("SpeciesName", "Code") )
  rownames(iucn.failed.df) <- NULL
  
  .write_logfile(iucn.failed.df, logfile = "failed.iucn.csv", project.name = project.name)
  .write_logfile(output.failed, logfile = "failed.gbif.csv", project.name = project.name)
  
  # reproject output --------------------------------------------------------
  
  this.X <- output.success$X
  this.Y <- output.success$Y
  
  this.projected <- project(cbind(this.X, this.Y),
                            from = crs("EPSG:4326"), to = crs("EPSG:3035"))
  
  output.success$X <- this.projected[,1]
  output.success$Y <- this.projected[,2]
  
  ## Output ------------------------------------------------------------
  
  output <- new("gbif_outsider")
  output@outsider <- output.success
  output@data.summary <- output.summary
  output@iucn.distrib <- iucn.list
  output@checklist <- checklist
  output@project.name <- project.name
  output@gbif.failed <- output.failed
  output@iucn.failed <- iucn.failed.df
  output
}

### Argument Check -----------------------------------

.detect_gbif_outsider.check.args <- function(checklist, folder.gbif, folder.iucn,
                                             filter.atlas, project.name, nb.cpu){
  .check_checklist(checklist)
  .fun_testIfInherits(folder.gbif, "character")
  .fun_testIfDirExists(folder.gbif)
  .fun_testIfInherits(folder.iucn, "character")
  .fun_testIfDirExists(folder.iucn)
  .fun_testIfPosInt(nb.cpu)
  if (missing(filter.atlas)) filter.atlas <- FALSE
  stopifnot(is.logical(filter.atlas))
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
              nrow(object@gbif.failed)
            median.dist <- median(object@outsider$distance_to_iucn)
            median.prop <- median(object@data.summary$outside.iucn /
                                    object@data.summary$total.gbif)
            cat("\n Project: ",object@project.name)
            cat("\n",  nrow(object@data.summary), 
                " species with gbif data outside IUCN ranges")
            cat("\n", goodspecies, 
                " species for which all gbif data were inside IUCN ranges (or without IUCN ranges")
            cat("\n", nrow(object@gbif.failed), 
                " species for which gbif data could not be retrieved")
            cat("\n", nrow(object@iucn.failed), 
                " species for which iucn data could not be retrieved")
            cat("\n", "Median distance to IUCN ranges across all data:",
                round(median.dist),"m")
            cat("\n", "Median proportion of points outside IUCN ranges across species:",
                round(median.prop*100, digits = 2),"%")
            invisible(NULL)
          })

### summary_outsider    --------------------------------------------------
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
  
  if (type == "Species") {
    return(object@data.summary)
  }
  
  object.summary <-
    object@data.summary  %>% 
    filter(outside.iucn > 0) %>% 
    group_by(across(type)) %>% 
    summarise(
      q5 = median(q5),
      q25 = median(q25),
      median = median(median),
      q75 = median(q75),
      q80 = median(q80),
      q85 = median(q85),
      q90 = median(q90),
      q95 = median(q95),
      .groups = "drop") 
  
  if (type == "Order") {
    object.summary <- object@outsider %>% 
      group_by(Class, Order) %>% 
      summarise(Code = first(Code),
                .groups = "drop") %>%
      select(-Code) %>% 
      right_join(object.summary, by = "Order") %>% 
      arrange(Class, Order)
  }
  if (type == "Family") {
    object.summary <- object@outsider %>% 
      group_by(Class, Order, Family) %>% 
      summarise(Code = first(Code),
                .groups = "drop") %>%
      select(-Code) %>%
      right_join(object.summary, by = "Family") %>% 
      arrange(Class, Order, Family)
  }
  object.summary
}

### plot.gbif_outsider  --------------------------------------------------
##' 
##' @rdname gbif_outsider
##' @export
##' @param x a \code{gbif_outsider} object
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
##' @importFrom grDevices cm

setMethod('plot', signature(x = 'gbif_outsider', y = 'missing'),
          function(x, type = "Class", nb.cpu = 1, plot.folder = "gbif_outsider",
                   buffer.config, species.buffer)
          {
            type.range <- c("Class","Order","Family", "Species")
            .fun_testIfIn(type, type.range)
            .fun_testIfPosInt(nb.cpu)
            .register_cluster(nb.cpu = nb.cpu)
            
            has.buffer.config <- !missing(buffer.config)
            has.species.buffer <- !missing(species.buffer)
            
            x.summary <- summary_outsider(x, type = type)
            if (type != "Species") {
              if (type != "Class") {
                x.summary <- arrange(x.summary, Class, Order) 
                x.summary <-
                  mutate(x.summary,
                         Class = factor(Class, levels = unique(x.summary$Class)),
                         Order = factor(Order, levels = unique(x.summary$Order)))
              }
              if (type == "Family") {
                x.summary <- arrange(x.summary, Class, Order, Family) 
                x.summary <-
                  mutate(x.summary,
                         Family = factor(Family, levels = unique(x.summary$Family)))
                
              }
              type.fill <- type.range[max(1, which(type.range == type) - 1)]
              nfill <- length(unique(unlist(x.summary[,type]))) + 1
              set.seed(42)
              col.pal <- gg_color_hue(nfill + 1)[sample(seq_len(nfill),
                                                        size = nfill, 
                                                        replace = FALSE)]
              g <- ggplot(x.summary) +
                geom_boxplot(aes(x = get(type),
                                 fill = get(type.fill),
                                 ymin = q5, ymax = q95,
                                 lower = q25, upper = q75,
                                 middle = median), stat = "identity") +
                scale_y_log10(
                  "Distance to IUCN distribution",
                  label = label_number(suffix = "m", 
                                       scale_cut = cut_short_scale()[1:2])) +
                scale_fill_manual(type.fill,
                                  values = col.pal) +
                theme(axis.text.x = element_text(angle = 90,
                                                 hjust = 1,
                                                 vjust = 0.5)) + 
                scale_x_discrete(type)
              
              if (type == "Family") {
                g <- g + 
                  facet_grid(~Class, scales = "free", space = "free")
              }
              return(g)
            } 
            # Species specifc maps plot
            dir.create(plot.folder, recursive = TRUE, showWarnings = FALSE)
            class.list <- unique(x@checklist$Class)
            folder.list <- c("0 to 25",
                             "25 to 75", 
                             "75 to 100")
            for (this.class in class.list) {
              for (this.folder in folder.list) {
                dir.create(paste0(plot.folder,'/',this.class,'/',this.folder),
                           recursive = TRUE, showWarnings = FALSE)
              }
            }
            x.summary <- 
              x.summary %>% 
              filter(outside.iucn > 0)
            
            listname <- x.summary$species
            names(listname) <- x.summary$Code
            out.list <- foreach( thiscode = names(listname) ) %dopar% {
              cli_progress_step(listname[thiscode])
              df <- x@outsider %>% 
                filter(Code == thiscode)
              this.summary <- filter(x.summary, Code == thiscode)
              
              path.file <- 
                paste0(
                  plot.folder, '/',
                  this.summary$Class, '/',
                  ifelse(this.summary$prop.outside < 25,
                         folder.list[1],
                         ifelse(this.summary$prop.outside < 75,
                                folder.list[2],
                                folder.list[3])), '/',
                  listname[thiscode], '.png'
                )
              
              this.iucn <- x@iucn.distrib[[thiscode]]
              g <- ggplot()
              if (!is.null(this.iucn) && 
                  grepl(x = this.iucn, pattern = ".tif")) {
                this.distrib <- rast(this.iucn)
                capture.output({
                  g <- ggplot(df) +
                    geom_spatraster(
                      data = mutate(this.distrib, 
                                    layer = factor(layer))
                    ) +
                    scale_fill_manual(
                      "IUCN distribution",
                      values = c("#d95f02", "#7570b3"),
                      na.value = "grey")
                }, type = "message")
              }
              if (nrow(df) > 0) {
                if (!has.species.buffer | !has.buffer.config) {
                  g <- g +
                    geom_point(data = df,
                               aes(x = X, y = Y, color = distance_to_iucn), 
                               shape = 20) 

                } else {
                  this.buffer <- buffer.config[[species.buffer[[listname[thiscode]]]]]*1000
                  g <- g +
                    geom_point(data  = filter(df, distance_to_iucn <= this.buffer),
                               aes(x = X, y = Y),
                               color = '#1b9e77', shape = 20) +
                    geom_point(data  = filter(df, distance_to_iucn > this.buffer),
                               aes(x = X, y = Y, color = distance_to_iucn), 
                               shape = 20) 
                  
                }
                g <- g +
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
              cli_progress_done()
              return(NULL)
            }
            invisible(NULL)
          })


