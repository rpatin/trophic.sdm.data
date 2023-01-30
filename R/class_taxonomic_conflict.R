## --------------------------------------------------------------------------- #
# 1. taxonomic_conflict ---------------------------------------------------
## --------------------------------------------------------------------------- #

##' @name taxonomic_conflict
##' @aliases taxonomic_conflict-class
##' @author Remi Patin
##' 
##' @title Object class describing taxonomic conflicts within trophic groups
##' 
##' @description Class returned by \code{\link{detect_taxonomic_conflict}}, 
##' that list species within the same trophic group with different taxonomy but 
##' the same predator
##' 
##' 
##' @param metaweb a \code{data.frame} with all species interactions listed
##' @param trophic.groups a \code{data.frame} assigning trophic groups to all 
##' species of interest
##' @param checklist a \code{data.frame} with information on all species of interest
##' @param project.name a \code{character} indicating the folder in which logfiles 
##' and data may be written
##' 
##' @slot class.summary a \code{data.frame} summarising the number of class-levels conflicts
##' @slot class.conflict a \code{data.frame} with all row from \code{metaweb} with 
##' class-level conflicts
##' @slot order.summary a \code{data.frame} summarising the number of order-levels conflicts
##' @slot order.conflict a \code{data.frame} with all row from \code{metaweb} with 
##' order-level conflicts
##' @slot metaweb a \code{data.frame} with all species interactions listed
##' @slot trophic.groups a \code{data.frame} with information on trophic groups
##' @slot checklist a \code{data.frame} with information on all species of interest
##' @slot project.name a \code{character} indicating the folder in which logfiles 
##' and data may be written
##' 
##' @examples
##' 
##' showClass("taxonomic_conflict")
NULL

##' @name taxonomic_conflict-class
##' @rdname taxonomic_conflict
##' @export
##' 

## 1.1 Class Definition ----------------------------------------------------------------------------
setClass("taxonomic_conflict",
         representation(class.summary = 'data.frame',
                        class.conflict = 'data.frame',
                        order.summary = 'data.frame',
                        order.conflict = 'data.frame',
                        family.summary = 'data.frame',
                        family.conflict = 'data.frame',
                        metaweb = "data.frame",
                        trophic.groups = "data.frame",
                        checklist = "data.frame",
                        project.name = "character"),
         validity = function(object){ 
           .check_metaweb(object@metaweb)
           .check_checklist(object@checklist)
           
           .check_metaweb(object@class.conflict)
           .fun_testIfIn(object@class.conflict$Prey_Code_new,
                         unique(object@metaweb$Prey_Code_new))
           .fun_testIfIn(object@class.conflict$Pred_Code_new,
                         unique(object@metaweb$Pred_Code_new))
           .fun_testIfIn(object@class.conflict$Prey_Code_new, 
                         unique(object@checklist$Code))
           .fun_testIfIn(object@class.conflict$Pred_Code_new, 
                         unique(object@checklist$Code))
           
           .check_metaweb(object@order.conflict)
           .fun_testIfIn(object@order.conflict$Prey_Code_new, 
                         unique(object@metaweb$Prey_Code_new))
           .fun_testIfIn(object@order.conflict$Pred_Code_new,
                         unique(object@metaweb$Pred_Code_new))
           .fun_testIfIn(object@order.conflict$Prey_Code_new, 
                         unique(object@checklist$Code))
           .fun_testIfIn(object@order.conflict$Pred_Code_new, 
                         unique(object@checklist$Code))
           
           .check_metaweb(object@family.conflict)
           .fun_testIfIn(object@family.conflict$Prey_Code_new, 
                         unique(object@metaweb$Prey_Code_new))
           .fun_testIfIn(object@family.conflict$Pred_Code_new,
                         unique(object@metaweb$Pred_Code_new))
           .fun_testIfIn(object@family.conflict$Prey_Code_new, 
                         unique(object@checklist$Code))
           .fun_testIfIn(object@family.conflict$Pred_Code_new, 
                         unique(object@checklist$Code))
           
           .check_trophic.groups(object@trophic.groups)
           
           .fun_testIfInherits(project.name, "character")
           
           TRUE
         })

## 1.2 Constructors -------------------------------------------------------------

### detect_taxonomic_conflict -----------------------------------
##' 
##' @rdname taxonomic_conflict
##' @importFrom cli cli_progress_step cli_progress_message
##' @importFrom dplyr select group_by filter mutate full_join left_join ungroup
##' @export
##' 

detect_taxonomic_conflict <- 
  function(metaweb, trophic.groups, checklist, project.name){
    cli_progress_step("Argument check")
    .detect_taxonomic_conflict.check.args(metaweb = metaweb,
                                          trophic.groups = trophic.groups,
                                          checklist = checklist,
                                          project.name = project.name)
    metaweb.name <- deparse(substitute(metaweb))
    trophic.groups.name <- deparse(substitute(trophic.groups))
    checklist.name <- deparse(substitute(checklist))
    
    cli_progress_step("Filtering {metaweb.name} and {trophic.groups.name} against {checklist.name}")
    metaweb <- .filter_df_by_checklist(metaweb,
                                       checklist, 
                                       col.name = "Prey_Code_new",
                                       project.name = project.name)  
    metaweb <- .filter_df_by_checklist(metaweb, 
                                       checklist,
                                       col.name = "Pred_Code_new",
                                       project.name = project.name)  
    trophic.groups <- .filter_df_by_checklist(trophic.groups, 
                                              checklist, 
                                              col.name = "Code",
                                              project.name = project.name)  
    
    cli_progress_step("Jointure of dataset")
    
    internal.groups <- 
      trophic.groups %>% 
      select(Code,group)
    internal.checklist <- 
      checklist %>% 
      select(Code, Class, Order, Family)
    grouped_metaweb <- metaweb %>% 
      select(Prey_Code_new,Pred_Code_new, Prey_Name, Pred_Name) %>% 
      left_join(internal.checklist, by = c("Prey_Code_new" = "Code")) %>% 
      left_join(internal.groups, by = c("Prey_Code_new" = "Code")) 
    
    cli_progress_step("Detection of class-level conflict")
    class.conflict <- 
      grouped_metaweb %>% 
      group_by(Pred_Code_new, group) %>% 
      mutate(nClass = length(unique(Class))) %>% 
      ungroup() %>% 
      filter(nClass > 1)
    
    cli_progress_step("Detection of order-level conflict")
    order.conflict <- 
      grouped_metaweb %>% 
      group_by(Pred_Code_new, group, Class) %>% 
      mutate(nOrder = length(unique(Order))) %>% 
      ungroup() %>% 
      filter(nOrder > 1)
    
    cli_progress_step("Detection of family-level conflict")
    family.conflict <- 
      grouped_metaweb %>% 
      group_by(Pred_Code_new, group, Class, Order) %>% 
      mutate(nFamily = length(unique(Family))) %>% 
      ungroup() %>% 
      filter(nFamily > 1)
    
    cli_progress_step("Conflict Summary")
    class.summary <- 
      class.conflict %>% 
      group_by(Pred_Code_new, group) %>% 
      summarise(nClass = first(nClass)) 
    
    order.summary <- 
      order.conflict %>% 
      group_by(Pred_Code_new, group, Class) %>% 
      summarise(nOrder = first(nOrder)) 
    
    family.summary <- 
      family.conflict %>% 
      group_by(Pred_Code_new, group, Class, Order) %>% 
      summarise(nFamily = first(nFamily)) 
    
    cli_progress_step("Creating taxonomic_conflict object")
    x <- new("taxonomic_conflict")
    x@class.summary <- class.summary
    x@class.conflict <- class.conflict
    x@order.summary <- order.summary
    x@order.conflict <- order.conflict
    x@family.summary <- family.summary
    x@family.conflict <- family.conflict
    x@metaweb <- metaweb
    x@trophic.groups <- internal.groups
    x@checklist <- internal.checklist
    x@project.name <- project.name
    x
  }

### Argument check  --------------------------------------------------

.detect_taxonomic_conflict.check.args <- 
  function(metaweb, trophic.groups, checklist, project.name){
    .check_metaweb(metaweb)
    .check_trophic.groups(trophic.groups)
    .check_checklist(checklist)
    .fun_testIfInherits(project.name, "character")
    TRUE
  }

## 1.3 Methods -------------------------------------------------------------
### show.taxonomic_conflict  --------------------------------------------------
##' 
##' @rdname taxonomic_conflict
##' @importMethodsFrom methods show
##' @export
##' 

setMethod('show', signature('taxonomic_conflict'),
          function(object)
          {
            cat("\n Project: ",object@project.name)
            cat("\n",  nrow(object@class.summary), 
                "class-level conflicts concerning",
                length(unique(object@class.summary$Pred_Code_new)), "predators")
            cat("\n",  nrow(object@order.summary), 
                "order-level conflicts concerning",
                length(unique(object@order.summary$Pred_Code_new)), "predators")
            cat("\n",  nrow(object@family.summary), 
                "family-level conflicts concerning",
                length(unique(object@family.summary$Pred_Code_new)), "predators")
          })

### summary_taxonomic_conflict  --------------------------------------------------
##' 
##' @rdname taxonomic_conflict
##' @param type a character, either 'Class', 'Order' or 'Family' determining
##' the level of conflict that needs to be summarised
##' @export
##' 

summary_conflict <- function(object, type = "Class") {
  .fun_testIfIn(type, c("Class","Order","Family"))
  .fun_testIfInherits(object, "taxonomic_conflict")
  pred.tot <- object@metaweb %>% 
    group_by(Pred_Code_new) %>% 
    summarise(ntot = n())
  
  if(type == "Class"){
    class.summary <- object@class.conflict %>% 
      group_by(Pred_Code_new, group, Class) %>% 
      summarise(Pred_Name = first(Pred_Name),
                n = n()) %>% 
      left_join(pred.tot, by = "Pred_Code_new")
    return(class.summary)
  }
  if(type == "Order"){
    order.summary <- object@order.conflict %>% 
      group_by(Pred_Code_new, group, Class, Order) %>% 
      summarise(Pred_Name = first(Pred_Name),
                n = n()) %>% 
      left_join(pred.tot, by = "Pred_Code_new")
    return(order.summary)
  }
  if(type == "Family"){
    family.summary <- object@family.conflict %>% 
      group_by(Pred_Code_new, group, Class, Order, Family) %>% 
      summarise(Pred_Name = first(Pred_Name),
                n = n()) %>% 
      left_join(pred.tot, by = "Pred_Code_new")
    return(family.summary)
  }
}

### plot.taxonomic_conflict  --------------------------------------------------
##' 
##' @rdname taxonomic_conflict
##' @param type a character, either 'Class', 'Order' or 'Family' determining
##' the level of conflict that needs to be summarised
##' @export
##' @importFrom ggplot2 ggplot geom_bar facet_grid aes scale_fill_discrete
##'   ggtitle scale_x_discrete scale_y_continuous theme element_text
##' @importFrom dplyr summarise group_by arrange
##' @importFrom magrittr "%>%"
##' 

setMethod('plot', signature(x = 'taxonomic_conflict', y = 'missing'),
          function(x, type = "Class")
          {
            .fun_testIfIn(type, c("Class","Order","Family"))
            x.summary <- summary_conflict(x, type = type)
            x.pred <- x.summary %>% 
              group_by(Pred_Code_new) %>% 
              summarise(ntot = first(ntot),
                        Pred_Name = first(Pred_Name))
            glist <- lapply(sort(unique(x.summary$group)), function(thisgroup){
              x.summary.filtered <- x.summary %>% 
                filter(group == thisgroup)
              x.pred.filtered <-
                semi_join(x.pred, x.summary.filtered,
                          by = "Pred_Name")
              if(type == "Class"){
                ordered_levels <- unique(x.summary.filtered$Class)
              } else if (type == "Order"){
                ordered_levels <- unique(arrange(x.summary.filtered, Class, Order)$Order)
              } else {
                ordered_levels <- unique(arrange(x.summary.filtered, Class, Order, Family)$Family)
              }
            
              g <- ggplot(x.summary.filtered) +
                geom_bar(data = x.pred.filtered, aes(x = Pred_Name, y = ntot), fill = "grey", stat = 'identity') +
                geom_bar(aes(x = Pred_Name, y = n, fill = get(type)), position = "stack", stat = 'identity') +
                scale_fill_discrete(paste0(type), breaks = ordered_levels) +
                scale_y_continuous("Number of prey") +
                scale_x_discrete("Predator")+
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
                ggtitle(paste0(type, "-level Conflicts - Trophic Group ", thisgroup))
              if (type == "Order") {
                g <- g + 
                  facet_grid(~Class, scales = "free", space = "free")
              }
              if (type == "Family") {
                g <- g + 
                  facet_grid(~Order, scales = "free", space = "free")
              }
              g
            })
            return(glist)
          })



